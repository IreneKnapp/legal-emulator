module Processor.CPU6502
  (
   CPU6502State(..),
   cpu6502PowerOnState,
   cpu6502Cycle
  )
  where

import Data.Bits
import Data.Word


data CPU6502State =
  CPU6502State {
      cpu6502StateProgramCounter :: Word16,
      cpu6502StateStackPointer :: Word8,
      cpu6502StateAccumulator :: Word8,
      cpu6502StateXIndexRegister :: Word8,
      cpu6502StateYIndexRegister :: Word8,
      cpu6502StateStatusRegister :: Word8,
      cpu6502StateInternalAdditionOverflow :: Bool,
      cpu6502StateInternalStoredAddress :: Word16,
      cpu6502StateInternalStoredValue :: Word8,
      cpu6502StateMicrocodeInstructionQueue :: [MicrocodeInstruction]
    }


data AddressSource
  = ProgramCounterAddressSource
  | FixedAddressSource Word16
  | StoredAddressSource


data ReadWrite = Read | Write


data InstructionMnemonic
  = ADC | AND | ASL | BBR | BBS | BCC | BCS | BEQ | BIT | BMI | BNE | BPL
  | BRA | BRK | BVC | BVS | CLC | CLD | CLI | CLV | CMP | CPX | CPY | DEC
  | DEX | DEY | EOR | INC | INX | INY | JMP | JSR | LDA | LDX | LDY | LSR
  | NOP | ORA | PHA | PHP | PHX | PHY | PLA | PLP | PLX | PLY | RMB | ROL
  | ROR | RTI | RTS | SBC | SEC | SED | SEI | SMB | STA | STX | STY | STZ
  | TAX | TAY | TRB | TSB | TSX | TXA | TXS | TYA


data AddressingMode
  = AccumulatorAddressing
  | ImmediateAddressing
  | AbsoluteAddressing
  | ZeroPageAddressing
  | ZeroPageXIndexedAddressing
  | ZeroPageYIndexedAddressing
  | AbsoluteXIndexedAddressing
  | AbsoluteYIndexedAddressing
  | XIndexedAbsoluteIndirectAddressing
  | ImpliedAddressing
  | RelativeAddressing
  | ZeroPageRelativeAddressing
  | XIndexedIndirectAddressing
  | IndirectYIndexedAddressing
  | AbsoluteIndirectAddressing
  | IndirectAddressing


data InternalRegister
  = ProgramCounterHighByte
  | ProgramCounterLowByte
  | StackPointer
  | Accumulator
  | XIndexRegister
  | YIndexRegister
  | StatusRegister
  | StoredAddressHighByte
  | StoredAddressLowByte
  | StoredValue


data ArithmeticOperation = ArithmeticOperation


data MicrocodeInstruction =
  MicrocodeInstruction {
      microcodeInstructionRegister :: Maybe InternalRegister,
      microcodeInstructionAddressSource :: AddressSource,
      microcodeInstructionAddressOffset :: Maybe InternalRegister,
      microcodeInstructionAddressAddOne :: Bool,
      microcodeInstructionReadWrite :: ReadWrite,
      microcodeInstructionArithmeticOperation :: Maybe ArithmeticOperation,
      microcodeInstructionDecodeOperation :: Bool,
      microcodeInstructionFixStoredAddressHighByte :: Bool,
      microcodeInstructionIncrementProgramCounter :: Bool
    }


cpu6502PowerOnState :: CPU6502State
cpu6502PowerOnState =
  CPU6502State {
      cpu6502StateProgramCounter = 0xC000,
      cpu6502StateStackPointer = 0x00,
      cpu6502StateAccumulator = 0x00,
      cpu6502StateXIndexRegister = 0x00,
      cpu6502StateYIndexRegister = 0x00,
      cpu6502StateStatusRegister = 0x00,
      cpu6502StateInternalAdditionOverflow = False,
      cpu6502StateInternalStoredAddress = 0x0000,
      cpu6502StateInternalStoredValue = 0x00,
      cpu6502StateMicrocodeInstructionQueue =
        [fetchOpcodeMicrocodeInstruction]
    }


cpu6502Cycle :: ((outerState -> Word16 -> (Word8, outerState)),
                 (outerState -> Word16 -> Word8 -> outerState),
                 (outerState -> CPU6502State),
                 (outerState -> CPU6502State -> outerState))
             -> outerState
             -> outerState
cpu6502Cycle (fetchByte, storeByte, getState, putState) outerState =
  let cpuState = getState outerState
      (maybeMicrocodeInstruction, microcodeInstructionQueue') =
        case cpu6502StateMicrocodeInstructionQueue cpuState of
          (first:rest) -> (Just first, rest)
          _ -> (Nothing, [])
      (cpuState', outerState') =
        case maybeMicrocodeInstruction of
          Nothing -> (cpuState, outerState)
          Just microcodeInstruction ->
            let effectiveAddress =
                  computeEffectiveAddress cpuState microcodeInstruction
                (fetchedByte, cpuState', outerState') =
                  case microcodeInstructionReadWrite microcodeInstruction of
                    Read -> let (fetchedByte, outerState') =
                                  fetchByte outerState
                                            effectiveAddress
                                cpuState' =
                                  case microcodeInstructionRegister
                                        microcodeInstruction of
                                    Nothing -> cpuState
                                    Just internalRegister ->
                                      computeStoreInternalRegister
                                       internalRegister cpuState fetchedByte
                            in (fetchedByte, cpuState', outerState')
                    Write -> let storedByte =
                                   case microcodeInstructionRegister
                                         microcodeInstruction of
                                     Nothing -> 0x00
                                     Just internalRegister ->
                                       computeInternalRegister
                                        internalRegister cpuState
                                 outerState' =
                                   storeByte outerState
                                             effectiveAddress
                                             storedByte
                             in (0x00, cpuState, outerState')
                programCounter =
                  cpu6502StateProgramCounter cpuState'
                programCounter' =
                  if microcodeInstructionIncrementProgramCounter
                      microcodeInstruction
                    then programCounter + 1
                    else programCounter
                cpuState'' = cpuState' {
                                 cpu6502StateProgramCounter = programCounter',
                                 cpu6502StateMicrocodeInstructionQueue =
                                   microcodeInstructionQueue'
                               }
            in (cpuState'', outerState')
      outerState'' = putState outerState' cpuState'
  in outerState''


computeEffectiveAddress :: CPU6502State -> MicrocodeInstruction -> Word16
computeEffectiveAddress cpuState microcodeInstruction =
  let baseAddress =
        case microcodeInstructionAddressSource
              microcodeInstruction of
          ProgramCounterAddressSource ->
            cpu6502StateProgramCounter cpuState
          FixedAddressSource address ->
            address
          StoredAddressSource ->
            cpu6502StateInternalStoredAddress cpuState
      addressOffset =
        case microcodeInstructionAddressOffset
              microcodeInstruction of
          Nothing -> 0
          Just internalRegister ->
            fromIntegral $ computeInternalRegister internalRegister cpuState
      possibleIncrement =
        if microcodeInstructionAddressAddOne microcodeInstruction
          then 1
          else 0
  in baseAddress + addressOffset + possibleIncrement


computeInternalRegister :: InternalRegister -> CPU6502State -> Word8
computeInternalRegister internalRegister cpuState =
  case internalRegister of
    ProgramCounterHighByte ->
      fromIntegral
      $ shiftR (cpu6502StateProgramCounter cpuState) 8 .&. 0xFF
    ProgramCounterLowByte ->
      fromIntegral
      $ shiftR (cpu6502StateProgramCounter cpuState) 0 .&. 0xFF
    StackPointer ->
      cpu6502StateStackPointer cpuState
    Accumulator ->
      cpu6502StateAccumulator cpuState
    XIndexRegister ->
      cpu6502StateXIndexRegister cpuState
    YIndexRegister ->
      cpu6502StateYIndexRegister cpuState
    StatusRegister ->
      cpu6502StateStatusRegister cpuState
    StoredAddressHighByte ->
      fromIntegral
      $ shiftR (cpu6502StateInternalStoredAddress cpuState) 8 .&. 0xFF
    StoredAddressLowByte ->
      fromIntegral
      $ shiftR (cpu6502StateInternalStoredAddress cpuState) 0 .&. 0xFF
    StoredValue ->
      cpu6502StateInternalStoredValue cpuState


computeStoreInternalRegister
    :: InternalRegister -> CPU6502State -> Word8 -> CPU6502State
computeStoreInternalRegister internalRegister cpuState byte =
  case internalRegister of
    ProgramCounterHighByte ->
      let otherByte =
            fromIntegral
            $ shiftR (cpu6502StateProgramCounter cpuState) 0 .&. 0xFF
          programCounter' =
            (shiftL (fromIntegral byte) 8)
            .|. (shiftL (fromIntegral otherByte) 0)
      in cpuState {
             cpu6502StateProgramCounter = programCounter'
           }
    ProgramCounterLowByte ->
      let otherByte =
            fromIntegral
            $ shiftR (cpu6502StateProgramCounter cpuState) 8 .&. 0xFF
          programCounter' =
            (shiftL (fromIntegral otherByte) 8)
            .|. (shiftL (fromIntegral byte) 0)
      in cpuState {
             cpu6502StateProgramCounter = programCounter'
           }
    StackPointer ->
      cpuState {
          cpu6502StateStackPointer = byte
        }
    Accumulator ->
      cpuState {
          cpu6502StateAccumulator = byte
        }
    XIndexRegister ->
      cpuState {
          cpu6502StateXIndexRegister = byte
        }
    YIndexRegister ->
      cpuState {
          cpu6502StateYIndexRegister = byte
        }
    StatusRegister ->
      cpuState {
          cpu6502StateStatusRegister = byte
        }
    StoredAddressHighByte ->
      let otherByte =
            fromIntegral
            $ shiftR (cpu6502StateInternalStoredAddress cpuState) 0 .&. 0xFF
          storedAddress' =
            (shiftL (fromIntegral byte) 8)
            .|. (shiftL (fromIntegral otherByte) 0)
      in cpuState {
             cpu6502StateInternalStoredAddress = storedAddress'
           }
    StoredAddressLowByte ->
      let otherByte =
            fromIntegral
            $ shiftR (cpu6502StateInternalStoredAddress cpuState) 8 .&. 0xFF
          storedAddress' =
            (shiftL (fromIntegral otherByte) 8)
            .|. (shiftL (fromIntegral byte) 0)
      in cpuState {
             cpu6502StateInternalStoredAddress = storedAddress'
           }
    StoredValue ->
      cpuState {
          cpu6502StateInternalStoredValue = byte
        }


decodeInstructionMnemonicAndAddressingMode
    :: Word8 -> Maybe (InstructionMnemonic, AddressingMode)
decodeInstructionMnemonicAndAddressingMode opcode =
  case opcode of
    0x00 -> Just (BRK, ImpliedAddressing)
    0x01 -> Just (ORA, XIndexedIndirectAddressing)
    0x02 -> Nothing
    0x03 -> Nothing
    0x04 -> Nothing
    0x05 -> Just (ORA, ZeroPageAddressing)
    0x06 -> Just (ASL, ZeroPageAddressing)
    0x07 -> Nothing
    0x08 -> Just (PHP, ImpliedAddressing)
    0x09 -> Just (ORA, ImmediateAddressing)
    0x0A -> Just (ASL, AccumulatorAddressing)
    0x0B -> Nothing
    0x0C -> Nothing
    0x0D -> Just (ORA, AbsoluteAddressing)
    0x0E -> Just (ASL, AbsoluteAddressing)
    0x0F -> Nothing
    0x10 -> Just (BPL, RelativeAddressing)
    0x11 -> Just (ORA, IndirectYIndexedAddressing)
    0x12 -> Nothing
    0x13 -> Nothing
    0x14 -> Nothing
    0x15 -> Just (ORA, ZeroPageXIndexedAddressing)
    0x16 -> Just (ASL, ZeroPageXIndexedAddressing)
    0x17 -> Nothing
    0x18 -> Just (CLC, ImpliedAddressing)
    0x19 -> Just (ORA, AbsoluteYIndexedAddressing)
    0x1A -> Nothing
    0x1B -> Nothing
    0x1C -> Nothing
    0x1D -> Just (ORA, AbsoluteXIndexedAddressing)
    0x1E -> Just (ASL, AbsoluteXIndexedAddressing)
    0x1F -> Nothing
    0x20 -> Just (JSR, AbsoluteAddressing)
    0x21 -> Just (AND, XIndexedIndirectAddressing)
    0x22 -> Nothing
    0x23 -> Nothing
    0x24 -> Just (BIT, ZeroPageAddressing)
    0x25 -> Just (AND, ZeroPageAddressing)
    0x26 -> Just (ROL, ZeroPageAddressing)
    0x27 -> Nothing
    0x28 -> Just (PLP, ImpliedAddressing)
    0x29 -> Just (AND, ImmediateAddressing)
    0x2A -> Just (ROL, AccumulatorAddressing)
    0x2B -> Nothing
    0x2C -> Just (BIT, AbsoluteAddressing)
    0x2D -> Just (AND, AbsoluteAddressing)
    0x2E -> Just (ROL, AbsoluteAddressing)
    0x2F -> Nothing
    0x30 -> Just (BMI, RelativeAddressing)
    0x31 -> Just (AND, IndirectYIndexedAddressing)
    0x32 -> Nothing
    0x33 -> Nothing
    0x34 -> Nothing
    0x35 -> Just (AND, ZeroPageXIndexedAddressing)
    0x36 -> Just (ROL, ZeroPageXIndexedAddressing)
    0x37 -> Nothing
    0x38 -> Just (SEC, ImpliedAddressing)
    0x39 -> Just (AND, AbsoluteYIndexedAddressing)
    0x3A -> Nothing
    0x3B -> Nothing
    0x3C -> Nothing
    0x3D -> Just (AND, AbsoluteXIndexedAddressing)
    0x3E -> Just (ROL, AbsoluteXIndexedAddressing)
    0x3F -> Nothing
    0x40 -> Just (RTI, ImpliedAddressing)
    0x41 -> Just (EOR, XIndexedIndirectAddressing)
    0x42 -> Nothing
    0x43 -> Nothing
    0x44 -> Nothing
    0x45 -> Just (EOR, ZeroPageAddressing)
    0x46 -> Just (LSR, ZeroPageAddressing)
    0x47 -> Nothing
    0x48 -> Just (PHA, ImpliedAddressing)
    0x49 -> Just (EOR, ImmediateAddressing)
    0x4A -> Just (LSR, AccumulatorAddressing)
    0x4B -> Nothing
    0x4C -> Just (JMP, AbsoluteAddressing)
    0x4D -> Just (EOR, AbsoluteAddressing)
    0x4E -> Just (LSR, AbsoluteAddressing)
    0x4F -> Nothing
    0x50 -> Just (BVC, RelativeAddressing)
    0x51 -> Just (EOR, IndirectYIndexedAddressing)
    0x52 -> Nothing
    0x53 -> Nothing
    0x54 -> Nothing
    0x55 -> Just (EOR, ZeroPageXIndexedAddressing)
    0x56 -> Just (LSR, ZeroPageXIndexedAddressing)
    0x57 -> Nothing
    0x58 -> Just (CLI, ImpliedAddressing)
    0x59 -> Just (EOR, AbsoluteYIndexedAddressing)
    0x5A -> Nothing
    0x5B -> Nothing
    0x5C -> Nothing
    0x5D -> Just (EOR, AbsoluteXIndexedAddressing)
    0x5E -> Just (LSR, AbsoluteXIndexedAddressing)
    0x5F -> Nothing
    0x60 -> Just (RTS, ImpliedAddressing)
    0x61 -> Just (ADC, XIndexedIndirectAddressing)
    0x62 -> Nothing
    0x63 -> Nothing
    0x64 -> Nothing
    0x65 -> Just (ADC, ZeroPageAddressing)
    0x66 -> Just (ROR, ZeroPageAddressing)
    0x67 -> Nothing
    0x68 -> Just (PLA, ImpliedAddressing)
    0x69 -> Just (ADC, ImmediateAddressing)
    0x6A -> Just (ROR, AccumulatorAddressing)
    0x6B -> Nothing
    0x6C -> Just (JMP, AbsoluteIndirectAddressing)
    0x6D -> Just (ADC, AbsoluteAddressing)
    0x6E -> Just (ROR, AbsoluteAddressing)
    0x6F -> Nothing
    0x70 -> Just (BVS, RelativeAddressing)
    0x71 -> Just (ADC, IndirectYIndexedAddressing)
    0x72 -> Nothing
    0x73 -> Nothing
    0x74 -> Nothing
    0x75 -> Just (ADC, ZeroPageXIndexedAddressing)
    0x76 -> Just (ROR, ZeroPageXIndexedAddressing)
    0x77 -> Nothing
    0x78 -> Just (SEI, ImpliedAddressing)
    0x79 -> Just (ADC, AbsoluteYIndexedAddressing)
    0x7A -> Nothing
    0x7B -> Nothing
    0x7C -> Nothing
    0x7D -> Just (ADC, AbsoluteXIndexedAddressing)
    0x7E -> Just (ROR, AbsoluteXIndexedAddressing)
    0x7F -> Nothing
    0x80 -> Nothing
    0x81 -> Just (STA, XIndexedIndirectAddressing)
    0x82 -> Nothing
    0x83 -> Nothing
    0x84 -> Just (STY, ZeroPageAddressing)
    0x85 -> Just (STA, ZeroPageAddressing)
    0x86 -> Just (STX, ZeroPageAddressing)
    0x87 -> Nothing
    0x88 -> Just (DEY, ImpliedAddressing)
    0x89 -> Nothing
    0x8A -> Just (TXA, ImpliedAddressing)
    0x8B -> Nothing
    0x8C -> Just (STY, AbsoluteAddressing)
    0x8D -> Just (STA, AbsoluteAddressing)
    0x8E -> Just (STX, AbsoluteAddressing)
    0x8F -> Nothing
    0x90 -> Just (BCC, RelativeAddressing)
    0x91 -> Just (STA, IndirectYIndexedAddressing)
    0x92 -> Nothing
    0x93 -> Nothing
    0x94 -> Just (STY, ZeroPageXIndexedAddressing)
    0x95 -> Just (STA, ZeroPageXIndexedAddressing)
    0x96 -> Just (STX, ZeroPageYIndexedAddressing)
    0x97 -> Nothing
    0x98 -> Just (TYA, ImpliedAddressing)
    0x99 -> Just (STA, AbsoluteYIndexedAddressing)
    0x9A -> Just (TXS, ImpliedAddressing)
    0x9B -> Nothing
    0x9C -> Nothing
    0x9D -> Just (STA, AbsoluteXIndexedAddressing)
    0x9E -> Nothing
    0x9F -> Nothing
    0xA0 -> Just (LDY, ImmediateAddressing)
    0xA1 -> Just (LDA, XIndexedIndirectAddressing)
    0xA2 -> Just (LDX, ImmediateAddressing)
    0xA3 -> Nothing
    0xA4 -> Just (LDY, ZeroPageAddressing)
    0xA5 -> Just (LDA, ZeroPageAddressing)
    0xA6 -> Just (LDX, ZeroPageAddressing)
    0xA7 -> Nothing
    0xA8 -> Just (TAY, ImpliedAddressing)
    0xA9 -> Just (LDA, ImmediateAddressing)
    0xAA -> Just (TAX, ImpliedAddressing)
    0xAB -> Nothing
    0xAC -> Just (LDY, AbsoluteAddressing)
    0xAD -> Just (LDA, AbsoluteAddressing)
    0xAE -> Just (LDX, AbsoluteAddressing)
    0xAF -> Nothing
    0xB0 -> Just (BCS, RelativeAddressing)
    0xB1 -> Just (LDA, IndirectYIndexedAddressing)
    0xB2 -> Nothing
    0xB3 -> Nothing
    0xB4 -> Just (LDY, ZeroPageXIndexedAddressing)
    0xB5 -> Just (LDA, ZeroPageXIndexedAddressing)
    0xB6 -> Just (LDX, ZeroPageYIndexedAddressing)
    0xB7 -> Nothing
    0xB8 -> Just (CLV, ImpliedAddressing)
    0xB9 -> Just (LDA, AbsoluteYIndexedAddressing)
    0xBA -> Just (TSX, ImpliedAddressing)
    0xBB -> Nothing
    0xBC -> Just (LDY, AbsoluteXIndexedAddressing)
    0xBD -> Just (LDA, AbsoluteXIndexedAddressing)
    0xBE -> Just (LDX, AbsoluteYIndexedAddressing)
    0xBF -> Nothing
    0xC0 -> Just (CPY, ImmediateAddressing)
    0xC1 -> Just (CMP, XIndexedIndirectAddressing)
    0xC2 -> Nothing
    0xC3 -> Nothing
    0xC4 -> Just (CPY, ZeroPageAddressing)
    0xC5 -> Just (CMP, ZeroPageAddressing)
    0xC6 -> Just (DEC, ZeroPageAddressing)
    0xC7 -> Nothing
    0xC8 -> Just (INY, ImpliedAddressing)
    0xC9 -> Just (CMP, ImmediateAddressing)
    0xCA -> Just (DEX, ImpliedAddressing)
    0xCB -> Nothing
    0xCC -> Just (CPY, AbsoluteAddressing)
    0xCD -> Just (CMP, AbsoluteAddressing)
    0xCE -> Just (DEC, AbsoluteAddressing)
    0xCF -> Nothing
    0xD0 -> Just (BNE, RelativeAddressing)
    0xD1 -> Just (CMP, IndirectYIndexedAddressing)
    0xD2 -> Nothing
    0xD3 -> Nothing
    0xD4 -> Nothing
    0xD5 -> Just (CMP, ZeroPageXIndexedAddressing)
    0xD6 -> Just (DEC, ZeroPageXIndexedAddressing)
    0xD7 -> Nothing
    0xD8 -> Just (CLD, ImpliedAddressing)
    0xD9 -> Just (CMP, AbsoluteYIndexedAddressing)
    0xDA -> Nothing
    0xDB -> Nothing
    0xDC -> Nothing
    0xDD -> Just (CMP, AbsoluteXIndexedAddressing)
    0xDE -> Just (DEC, AbsoluteXIndexedAddressing)
    0xDF -> Nothing
    0xE0 -> Just (CPX, ImmediateAddressing)
    0xE1 -> Just (SBC, XIndexedIndirectAddressing)
    0xE2 -> Nothing
    0xE3 -> Nothing
    0xE4 -> Just (CPX, ZeroPageAddressing)
    0xE5 -> Just (SBC, ZeroPageAddressing)
    0xE6 -> Just (INC, ZeroPageAddressing)
    0xE7 -> Nothing
    0xE8 -> Just (INX, ImpliedAddressing)
    0xE9 -> Just (SBC, ImmediateAddressing)
    0xEA -> Just (NOP, ImpliedAddressing)
    0xEB -> Nothing
    0xEC -> Just (CPX, AbsoluteAddressing)
    0xED -> Just (SBC, AbsoluteAddressing)
    0xEE -> Just (INC, AbsoluteAddressing)
    0xEF -> Nothing
    0xF0 -> Just (BEQ, RelativeAddressing)
    0xF1 -> Just (SBC, IndirectYIndexedAddressing)
    0xF2 -> Nothing
    0xF3 -> Nothing
    0xF4 -> Nothing
    0xF5 -> Just (SBC, ZeroPageXIndexedAddressing)
    0xF6 -> Just (INC, ZeroPageXIndexedAddressing)
    0xF7 -> Nothing
    0xF8 -> Just (SED, ImpliedAddressing)
    0xF9 -> Just (SBC, AbsoluteYIndexedAddressing)
    0xFA -> Nothing
    0xFB -> Nothing
    0xFC -> Nothing
    0xFD -> Just (SBC, AbsoluteXIndexedAddressing)
    0xFE -> Just (INC, AbsoluteXIndexedAddressing)
    0xFF -> Nothing


fetchOpcodeMicrocodeInstruction :: MicrocodeInstruction
fetchOpcodeMicrocodeInstruction =
  MicrocodeInstruction {
      microcodeInstructionRegister = Nothing,
      microcodeInstructionAddressSource = ProgramCounterAddressSource,
      microcodeInstructionAddressOffset = Nothing,
      microcodeInstructionAddressAddOne = False,
      microcodeInstructionReadWrite = Read,
      microcodeInstructionArithmeticOperation = Nothing,
      microcodeInstructionDecodeOperation = True,
      microcodeInstructionFixStoredAddressHighByte = False,
      microcodeInstructionIncrementProgramCounter = True
    }
