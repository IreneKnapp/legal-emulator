module Main (main) where

import Data.Array.IArray
import Data.Bits
import Data.Word

import FileFormat.INES
import Motherboard.NES
import Processor.CPU6502


main :: IO ()
main = do
  maybeHardwareState <- 
    readINESFile "/Users/dankna/Projects/legal-emulator/Tests/nestest.nes"
  case maybeHardwareState of
    Nothing -> putStrLn $ "Failed to load."
    Just hardwareState -> do
      putStrLn $ "Mapper " ++ (show $ hardwareStateMapperNumber hardwareState)
      putStrLn $ "PRG ROM "
                 ++ (show
                     $ (1+)
                     $ snd
                     $ bounds
                     $ hardwareStateProgramReadOnlyMemory hardwareState)
      putStrLn $ "CHR ROM "
                 ++ (show
                     $ (1+)
                     $ snd
                     $ bounds
                     $ hardwareStateCharacterReadOnlyMemory hardwareState)
      let loop i softwareState = do
            if i == 0
              then return ()
              else do
                let cpuState = softwareStateCPUState softwareState
                    programCounter = cpu6502StateProgramCounter cpuState
                putStrLn $ (showHexWord16 programCounter)
                softwareState <- return $ cpuCycle hardwareState softwareState
                loop (i - 1) softwareState
      loop 1000 nesPowerOnSoftwareState


showHexWord16 :: Word16 -> String
showHexWord16 word =
  let highByte = fromIntegral $ (shiftR word 8) .&. 0xFF
      lowByte = fromIntegral $ (shiftR word 0) .&. 0xFF
  in (showHexWord8 highByte) ++ (showHexWord8 lowByte)


showHexWord8 :: Word8 -> String
showHexWord8 byte =
  let highNibble = (shiftR byte 4) .&. 0xF
      lowNibble = (shiftR byte 0) .&. 0xF
      showHexNibble nibble =
        [(['0' .. '9'] ++ ['A' .. 'F']) !! fromIntegral nibble]
  in (showHexNibble highNibble) ++ (showHexNibble lowNibble)
