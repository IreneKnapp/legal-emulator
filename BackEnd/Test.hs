module Main (main) where

import Data.Array.IArray
import Data.Bits
import Data.List
import Data.Word

import FileFormat.INES
import Motherboard.NES
import PPU.PPU_NES
import Processor.CPU_6502


main :: IO ()
main = do
  maybeHardwareState <- 
    readINESFile "/Users/dankna/Projects/legal-emulator/Tests/nestest.nes"
  case maybeHardwareState of
    Nothing -> putStrLn $ "Failed to load."
    Just hardwareState -> do
      {-
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
      -}
      let loop i softwareState = do
            if i == 0
              then return ()
              else do
                let cpuState = softwareStateCPUState softwareState
                    atInstructionStart =
                      cpu6502AtInstructionStart cpuState
                      && motherboardAtCPUCycle hardwareState softwareState
                if atInstructionStart
                  then do
                    let programCounter = cpu6502StateProgramCounter cpuState
                        debugFetch = cpuDebugFetch hardwareState softwareState
                        opcode = debugFetch programCounter
                    case cpu6502DecodeInstructionMnemonicAndAddressingMode
                          opcode of
                      Nothing -> do
                        putStrLn $ (showHexWord16 programCounter)
                                   ++ "  Invalid instruction."
                      Just (instructionMnemonic, addressingMode) -> do
                        let nBytes = cpu6502NBytes addressingMode
                            byte2 = if nBytes >= 2
                                      then debugFetch $ programCounter + 1
                                      else undefined
                            byte3 = if nBytes >= 3
                                      then debugFetch $ programCounter + 2
                                      else undefined
                            bytes = case nBytes of
                                      1 -> [opcode]
                                      2 -> [opcode, byte2]
                                      3 -> [opcode, byte2, byte3]
                            ppuState = softwareStatePPUState softwareState
                            instructionCharacter =
                              characterizeMnemonic instructionMnemonic
                            addressReport = showHexWord16 programCounter
                            byteReport =
                              rightPad (intercalate " "
                                                    $ map showHexWord8 bytes)
                                       8
                            (lvalueSubreport, maybeRValueAddress) =
                              case addressingMode of
                                AccumulatorAddressing ->
                                  (show addressingMode, Nothing)
                                ImmediateAddressing ->
                                  ("#$"
                                   ++ showHexWord8 byte2,
                                   Nothing)
                                AbsoluteAddressing ->
                                  ("$"
                                   ++ showHexWord8 byte3
                                   ++ showHexWord8 byte2,
                                   Just $ shiftL (fromIntegral byte3) 8
                                          .|. fromIntegral byte2)
                                ZeroPageAddressing ->
                                  ("$"
                                   ++ showHexWord8 byte2,
                                   Just $ fromIntegral byte2)
                                ZeroPageXIndexedAddressing ->
                                  (show addressingMode,
                                   Nothing)
                                ZeroPageYIndexedAddressing ->
                                  (show addressingMode,
                                   Nothing)
                                AbsoluteXIndexedAddressing ->
                                  (show addressingMode,
                                   Nothing)
                                AbsoluteYIndexedAddressing ->
                                  (show addressingMode,
                                   Nothing)
                                ImpliedAddressing ->
                                  ("",
                                   Nothing)
                                RelativeAddressing ->
                                  let effectiveAddress
                                       = programCounter
                                         + (fromIntegral byte2)
                                         + (fromIntegral nBytes)
                                  in ("$" ++ showHexWord16 effectiveAddress,
                                      Nothing)
                                XIndexedIndirectAddressing ->
                                  (show addressingMode,
                                   Nothing)
                                IndirectYIndexedAddressing ->
                                  (show addressingMode,
                                   Nothing)
                                AbsoluteIndirectAddressing ->
                                  (show addressingMode,
                                   Nothing)
                            showRValueSubreport =
                              ((instructionCharacter == WriteCharacter)
                               || (instructionCharacter == ReadWriteCharacter)
                               || (instructionMnemonic == BIT))
                              && (mnemonicRegister instructionMnemonic
                                  /= NoRegister)
                            rvalue = case maybeRValueAddress of
                                       Nothing -> 0x00
                                       Just rvalueAddress ->
                                         debugFetch rvalueAddress
                            rvalueSubreport = " = " ++ showHexWord8 rvalue
                            disassemblyReport =
                              rightPad (show instructionMnemonic
                                        ++ " "
                                        ++ lvalueSubreport
                                        ++ if showRValueSubreport
                                             then rvalueSubreport
                                             else "")
                                       30
                            stateReport =
                              intercalate
                               " "
                               $ map (\(label, value) -> label ++ ":" ++ value)
                                     [("A",
                                       showHexWord8
                                        $ cpu6502StateAccumulator cpuState),
                                      ("X",
                                       showHexWord8
                                        $ cpu6502StateXIndexRegister cpuState),
                                      ("Y",
                                       showHexWord8
                                        $ cpu6502StateYIndexRegister cpuState),
                                      ("P",
                                       showHexWord8
                                        $ cpu6502StateStatusRegister cpuState
                                          .|. 0x20),
                                      ("SP",
                                       showHexWord8
                                        $ cpu6502StateStackPointer cpuState),
                                      ("CYC",
                                       leftPad
                                        (show
                                          $ ppuNESStateHorizontalClock ppuState)
                                        3),
                                      ("SL",
                                       show
                                        $ ppuNESStateVerticalClock ppuState)]
                        putStrLn $ intercalate "  "
                                               [addressReport,
                                                byteReport,
                                                disassemblyReport,
                                                stateReport]
                  else return ()
                softwareState
                  <- return $ motherboardCycle hardwareState softwareState
                loop (i - 1) softwareState
      loop 100000 motherboardPowerOnSoftwareState


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


leftPad :: String -> Int -> String
leftPad string width = padding (width - length string) ++ string


rightPad :: String -> Int -> String
rightPad string width = string ++ padding (width - length string)


padding :: Int -> String
padding width = take width $ repeat ' '
