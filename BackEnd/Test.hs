module Main (main) where

import Data.Array.IArray
import Data.Bits
import Data.Int
import Data.List
import Data.Maybe
import Data.Word

import FileFormat.INES
import Motherboard.NES
import qualified PPU.PPU_NES as PPU
import qualified Processor.CPU_6502 as CPU


main :: IO ()
main = do
  maybeHardwareState <- 
    readINESFile "/Users/dankna/Projects/legal-emulator/Tests/smb1.nes"
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
      let loop begun state = do
            let softwareState = stateSoftwareState state
                motherboardClock =
                  softwareStateMotherboardClockCount softwareState
                ppuState = softwareStatePPUState softwareState
                horizontalClock = ppuNESStateHorizontalClock ppuState
                verticalClock = ppuNESStateVerticalClock ppuState
            if begun
               && (motherboardClock == 0)
               && (horizontalClock == 340)
               && (verticalClock == 259)
              then return ()
              else loop True $ motherboardCycle state
      state <- loop False
                    $ State {
                          stateHardwareState = hardwareState,
                          stateSoftwareState = motherboardPowerOnSoftwareState
                        }
      putStrLn $ "Done."

      {-
      let loop i softwareState = do
            if i == 0
              then return ()
              else do
                let cpuState = softwareStateCPUState softwareState
                    atInstructionStart =
                      CPU.atInstructionStart cpuState
                      && motherboardAtCPUCycle hardwareState softwareState
                if atInstructionStart
                  then do
                    let programCounter = cpu6502StateProgramCounter cpuState
                        debugFetch = cpuDebugFetch hardwareState softwareState
                        opcode = debugFetch programCounter
                    case CPU.decodeInstructionMnemonicAndAddressingMode
                          opcode of
                      Nothing -> do
                        putStrLn $ (showHexWord16 programCounter)
                                   ++ "  Invalid instruction."
                      Just (instructionMnemonic, addressingMode, extended)
                       -> do
                        let nBytes = CPU.nBytes addressingMode
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
                                  ("A", Nothing)
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
                                  let x = cpu6502StateXIndexRegister cpuState
                                  in ("$"
                                      ++ showHexWord8 byte2
                                      ++ ",X @ "
                                      ++ showHexWord8 (byte2 + x),
                                      Just $ fromIntegral $ byte2 + x)
                                ZeroPageYIndexedAddressing ->
                                  let y = cpu6502StateYIndexRegister cpuState
                                  in ("$"
                                      ++ showHexWord8 byte2
                                      ++ ",Y @ "
                                      ++ showHexWord8 (byte2 + y),
                                      Just $ fromIntegral $ byte2 + y)
                                AbsoluteXIndexedAddressing ->
                                  let x = cpu6502StateXIndexRegister cpuState
                                      indexed =
                                        fromIntegral byte2
                                        + shiftL (fromIntegral byte3) 8
                                      effective = indexed + fromIntegral x
                                  in ("$"
                                      ++ showHexWord16 indexed
                                      ++ ",X @ "
                                      ++ showHexWord16 effective,
                                      Just effective)
                                AbsoluteYIndexedAddressing ->
                                  let y = cpu6502StateYIndexRegister cpuState
                                      indexed =
                                        fromIntegral byte2
                                        + shiftL (fromIntegral byte3) 8
                                      effective = indexed + fromIntegral y
                                  in ("$"
                                      ++ showHexWord16 indexed
                                      ++ ",Y @ "
                                      ++ showHexWord16 effective,
                                      Just effective)
                                ImpliedAddressing ->
                                  ("",
                                   Nothing)
                                RelativeAddressing ->
                                  let programCounterInt =
                                        fromIntegral programCounter
                                        + nBytes
                                        :: Int
                                      offsetInt =
                                        fromIntegral
                                         $ (fromIntegral byte2 :: Int8)
                                        :: Int
                                      effectiveAddress
                                       = fromIntegral
                                         $ programCounterInt + offsetInt
                                  in ("$" ++ showHexWord16 effectiveAddress,
                                      Nothing)
                                XIndexedIndirectAddressing ->
                                  let x = cpu6502StateXIndexRegister cpuState
                                      effective =
                                        (fromIntegral
                                          $ debugFetch
                                             $ fromIntegral
                                                $ byte2 + x)
                                        + shiftL (fromIntegral
                                                   $ debugFetch
                                                      $ fromIntegral
                                                         $ byte2 + x + 1)
                                                 8
                                  in ("($"
                                      ++ showHexWord8 byte2
                                      ++ ",X) @ "
                                      ++ showHexWord8 (byte2 + x)
                                      ++ " = "
                                      ++ showHexWord16 effective,
                                      Just effective)
                                IndirectYIndexedAddressing ->
                                  let y = cpu6502StateYIndexRegister cpuState
                                      indirectLow =
                                        debugFetch $ fromIntegral $ byte2
                                      indirectHigh =
                                        debugFetch $ fromIntegral $ byte2 + 1
                                      indirect =
                                        fromIntegral indirectLow
                                        + shiftL (fromIntegral indirectHigh) 8
                                      effective =
                                        indirect + fromIntegral y
                                  in ("($"
                                      ++ showHexWord8 byte2
                                      ++ "),Y = "
                                      ++ showHexWord16 indirect
                                      ++ " @ "
                                      ++ showHexWord16 effective,
                                      Just effective)
                                AbsoluteIndirectAddressing ->
                                  let indirect =
                                        fromIntegral byte2
                                        + shiftL (fromIntegral byte3) 8
                                      effective =
                                        (fromIntegral
                                          $ debugFetch indirect)
                                        + shiftL
                                           (fromIntegral
                                             $ debugFetch $ indirect + 1)
                                           8
                                  in ("($"
                                      ++ showHexWord16 indirect
                                      ++ ") = "
                                      ++ showHexWord16 effective,
                                      Just effective)
                            showRValueSubreport =
                              isJust maybeRValueAddress
                              && (((elem instructionCharacter
                                        [ReadCharacter,
                                         ReadWriteCharacter,
                                         WriteCharacter])
                                   && (mnemonicRegister instructionMnemonic
                                       /= NoRegister))
                                  || (extended
                                      && (instructionMnemonic == NOP)))
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
                            report = addressReport
                                     ++ "  "
                                     ++ byteReport
                                     ++ (if extended
                                           then " *"
                                           else "  ")
                                     ++ disassemblyReport
                                     ++ "  "
                                     ++ stateReport
                        putStrLn report
                  else return ()
                softwareState
                  <- return $ motherboardCycle hardwareState softwareState
                loop (i - 1) softwareState
      loop 2000000 motherboardPowerOnSoftwareState
      -}


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
