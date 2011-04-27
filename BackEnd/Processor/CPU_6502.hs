module Processor.CPU_6502
  (
   CPU_6502_State(..),
   InstructionMnemonic(..),
   AddressingMode(..),
   InternalRegister(..),
   InstructionCharacter(..),
   cpu6502PowerOnState,
   cpu6502Cycle,
   computeInternalRegister,
   cpu6502DecodeInstructionMnemonicAndAddressingMode,
   characterizeMnemonic,
   mnemonicRegister,
   cpu6502NBytes,
   cpu6502AtInstructionStart
  )
  where

import Data.Bits
import Data.Int
import Data.Word


data CPU_6502_State =
  CPU_6502_State {
      cpu6502StateProgramCounter :: Word16,
      cpu6502StateStackPointer :: Word8,
      cpu6502StateAccumulator :: Word8,
      cpu6502StateXIndexRegister :: Word8,
      cpu6502StateYIndexRegister :: Word8,
      cpu6502StateStatusRegister :: Word8,
      cpu6502StateInternalOverflow :: Bool,
      cpu6502StateInternalNegative :: Bool,
      cpu6502StateInternalStoredAddress :: Word16,
      cpu6502StateInternalLatch :: Word8,
      cpu6502StateMicrocodeInstructionQueue :: [MicrocodeInstruction]
    }


data AddressSource
  = ProgramCounterAddressSource
  | FixedAddressSource Word16
  | StoredAddressSource
  deriving (Eq, Show)


data ReadWrite = Read | Write
               deriving (Eq, Show)


data InstructionMnemonic
  = ADC | AND | ASL | BCC | BCS | BEQ | BIT | BMI | BNE | BPL | BRK | BVC
  | BVS | CLC | CLD | CLI | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR
  | INC | INX | INY | JMP | JSR | LDA | LDX | LDY | LSR | NOP | ORA | PHA
  | PHP | PLA | PLP | ROL | ROR | RTI | RTS | SBC | SEC | SED | SEI | STA
  | STX | STY | TAX | TAY | TSX | TXA | TXS | TYA
  deriving (Eq, Show)


data AddressingMode
  = AccumulatorAddressing
  | ImmediateAddressing
  | AbsoluteAddressing
  | ZeroPageAddressing
  | ZeroPageXIndexedAddressing
  | ZeroPageYIndexedAddressing
  | AbsoluteXIndexedAddressing
  | AbsoluteYIndexedAddressing
  | ImpliedAddressing
  | RelativeAddressing
  | XIndexedIndirectAddressing
  | IndirectYIndexedAddressing
  | AbsoluteIndirectAddressing
  deriving (Eq, Show)


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
  | Latch
  | NoRegister
  deriving (Eq, Show)


data ArithmeticOperation
  = ArithmeticIdentity
  | ArithmeticAnd
  | ArithmeticInclusiveOr
  | ArithmeticExclusiveOr
  | ArithmeticAdd
  | ArithmeticSubtract
  | ArithmeticCompare
  | ArithmeticBitCompare
  deriving (Eq, Show)


data Condition
  = CarryClear
  | CarrySet
  | NotEqual
  | Equal
  | Plus
  | Minus
  | OverflowClear
  | OverflowSet
  | InternalOverflowSet
  deriving (Eq, Show)


data InstructionCharacter
  = StackCharacter
  | ControlCharacter
  | RegisterCharacter
  | ReadCharacter
  | ReadWriteCharacter
  | WriteCharacter
  deriving (Eq, Show)


data IncrementDecrement = Increment | Decrement
                        deriving (Eq, Show)


data SetClear = Set | Clear
              deriving (Eq, Show)


data Transformation
  = ArithmeticShiftLeft
  | LogicalShiftRight
  | RotateLeft
  | RotateRight
  | IncrementDecrement IncrementDecrement
  deriving (Eq, Show)


data MicrocodeInstruction =
  MicrocodeInstruction {
      microcodeInstructionConditional
        :: Maybe (Condition, [MicrocodeInstruction], [MicrocodeInstruction]),
      microcodeInstructionInsteadFixProgramCounterHighByteIfNecessary
        :: Bool,
      microcodeInstructionRegister :: Maybe InternalRegister,
      microcodeInstructionRegisterFromLatch :: Maybe InternalRegister,
      microcodeInstructionAddLatchToProgramCounterLowByte :: Bool,
      microcodeInstructionAddressSource :: AddressSource,
      microcodeInstructionAddressOffset :: Maybe InternalRegister,
      microcodeInstructionAddressAddOne :: Bool,
      microcodeInstructionSettingStoredValueBits :: Maybe Word8,
      microcodeInstructionClearingFetchedValueBits :: Maybe Word8,
      microcodeInstructionReadWrite :: ReadWrite,
      microcodeInstructionArithmeticOperation :: Maybe ArithmeticOperation,
      microcodeInstructionDecodeOperation :: Bool,
      microcodeInstructionIncrementProgramCounter :: Bool,
      microcodeInstructionZeroStoredAddressHighByte :: Bool,
      microcodeInstructionAddRegisterToStoredAddress :: Maybe InternalRegister,
      microcodeInstructionFixStoredAddressHighByte :: Bool,
      microcodeInstructionStackPointerOperation :: Maybe IncrementDecrement,
      microcodeInstructionXIndexRegisterOperation :: Maybe IncrementDecrement,
      microcodeInstructionYIndexRegisterOperation :: Maybe IncrementDecrement,
      microcodeInstructionStatusRegisterOperation :: Maybe (SetClear, Word8),
      microcodeInstructionAccumulatorOperation
        :: Maybe Transformation,
      microcodeInstructionLatchOperation
        :: Maybe Transformation,
      microcodeInstructionRegisterRegisterCopy
        :: Maybe (InternalRegister, InternalRegister),
      microcodeInstructionUpdateStatusForRegister :: Maybe InternalRegister
    }
    deriving (Show)


cpu6502PowerOnState :: CPU_6502_State
cpu6502PowerOnState =
  CPU_6502_State {
      cpu6502StateProgramCounter = 0xC000,
      cpu6502StateStackPointer = 0xFD,
      cpu6502StateAccumulator = 0x00,
      cpu6502StateXIndexRegister = 0x00,
      cpu6502StateYIndexRegister = 0x00,
      cpu6502StateStatusRegister = 0x04,
      cpu6502StateInternalOverflow = False,
      cpu6502StateInternalNegative = False,
      cpu6502StateInternalStoredAddress = 0x0000,
      cpu6502StateInternalLatch = 0x00,
      cpu6502StateMicrocodeInstructionQueue =
        [fetchOpcodeMicrocodeInstruction]
    }


cpu6502Cycle :: ((outerState -> Word16 -> (Word8, outerState)),
                 (outerState -> Word16 -> Word8 -> outerState),
                 (outerState -> CPU_6502_State),
                 (outerState -> CPU_6502_State -> outerState))
             -> outerState
             -> outerState
cpu6502Cycle (fetchByte, storeByte, getState, putState) outerState =
  let checkForSetting =
        microcodeInstructionSettingStoredValueBits
      checkForClearing =
        microcodeInstructionClearingFetchedValueBits
      checkForInstead =
        microcodeInstructionInsteadFixProgramCounterHighByteIfNecessary
      checkForAddLatch =
        microcodeInstructionAddLatchToProgramCounterLowByte
      cpuState = getState outerState
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
                    Read ->
                      let (fetchedByte, outerState') =
                            fetchByte outerState effectiveAddress
                          fetchedByte' =
                            case checkForClearing microcodeInstruction of
                              Nothing -> fetchedByte
                              Just bits -> fetchedByte .&. complement bits
                          cpuState' =
                            case microcodeInstructionRegister
                                  microcodeInstruction of
                              Nothing -> cpuState
                              Just internalRegister ->
                                let maybeArithmetic =
                                      microcodeInstructionArithmeticOperation
                                       microcodeInstruction
                                    registerByte =
                                      computeInternalRegister
                                       internalRegister cpuState
                                    status =
                                      cpu6502StateStatusRegister cpuState
                                    (status', newByte) =
                                      case maybeArithmetic of
                                        Nothing -> (status, fetchedByte')
                                        Just arithmetic ->
                                          performArithmetic arithmetic
                                                            status
                                                            registerByte
                                                            fetchedByte'
                                    statusPostprocessor cpuState' =
                                      if internalRegister == StatusRegister
                                        then cpuState'
                                        else cpuState' {
                                                 cpu6502StateStatusRegister =
                                                   status'
                                               }
                                in statusPostprocessor
                                    $ computeStoreInternalRegister
                                       internalRegister cpuState newByte
                      in (fetchedByte', cpuState', outerState')
                    Write -> let storedByte' =
                                   case microcodeInstructionRegister
                                         microcodeInstruction of
                                     Nothing -> 0x00
                                     Just internalRegister ->
                                       computeInternalRegister
                                        internalRegister cpuState
                                 storedByte =
                                   case checkForSetting microcodeInstruction of
                                     Nothing -> storedByte'
                                     Just bits -> storedByte' .|. bits
                                 outerState' =
                                   storeByte outerState
                                             effectiveAddress
                                             storedByte
                             in (0x00, cpuState, outerState')
                cpuState'' =
                  case microcodeInstructionRegisterFromLatch
                        microcodeInstruction of
                    Nothing -> cpuState'
                    Just internalRegister ->
                      computeStoreInternalRegister internalRegister
                                                   cpuState'
                                                   $ cpu6502StateInternalLatch
                                                      cpuState'
                programCounter =
                  cpu6502StateProgramCounter cpuState''
                keepProgramCounter =
                  (programCounter, False, False, False)
                incrementProgramCounter =
                  (programCounter + 1, False, False, False)
                incrementProgramCounterOnePage =
                  (programCounter + 0x0100, True, False, False)
                decrementProgramCounterOnePage =
                  (programCounter - 0x0100, True, False, False)
                (programCounter',
                 programCounterHighByteWasWrong,
                 internalOverflowA',
                 internalNegativeA') =
                  if checkForInstead microcodeInstruction
                    then let internalOverflow =
                               cpu6502StateInternalOverflow cpuState
                             internalNegative =
                               cpu6502StateInternalNegative cpuState
                         in if internalOverflow
                              then if internalNegative
                                     then decrementProgramCounterOnePage
                                     else incrementProgramCounterOnePage
                              else
                                if microcodeInstructionIncrementProgramCounter
                                    microcodeInstruction
                                  then incrementProgramCounter
                                  else keepProgramCounter
                    else if microcodeInstructionIncrementProgramCounter
                             microcodeInstruction
                           then incrementProgramCounter
                           else
                             if checkForAddLatch microcodeInstruction
                               then let latch =
                                          cpu6502StateInternalLatch cpuState''
                                        latchInt8 =
                                          fromIntegral latch :: Int8
                                        latchInt =
                                          fromIntegral latch :: Int
                                        programCounterHighByte =
                                           programCounter .&. 0xFF00
                                        programCounterLowByte =
                                          fromIntegral
                                           (programCounter .&. 0x00FF)
                                          :: Int
                                        programCounterLowByteInt' =
                                          programCounterLowByte + latchInt
                                        internalNegative =
                                          (programCounterLowByteInt' < 0x00)
                                        internalOverflow =
                                          internalNegative
                                          || (programCounterLowByteInt' > 0xFF)
                                        programCounterLowByte' =
                                          fromIntegral
                                           programCounterLowByteInt'
                                          :: Word8
                                        programCounter' =
                                          fromIntegral programCounterLowByte'
                                          .|. programCounterHighByte
                                    in (programCounter',
                                        False,
                                        internalOverflow,
                                        internalNegative)
                               else keepProgramCounter
                storedAddress =
                  cpu6502StateInternalStoredAddress cpuState''
                (storedAddress',
                 internalOverflowB',
                 internalNegativeB') =
                  case microcodeInstructionAddRegisterToStoredAddress
                        microcodeInstruction of
                    Nothing -> (storedAddress, False, False)
                    Just register ->
                      let addend =
                            computeInternalRegister register cpuState''
                          addendInt8 =
                            fromIntegral addend :: Int8
                          addendInt =
                            fromIntegral addend :: Int
                          storedAddressHighByte =
                             storedAddress .&. 0xFF00
                          storedAddressLowByte =
                            fromIntegral
                             (storedAddress .&. 0x00FF)
                            :: Int
                          storedAddressLowByteInt' =
                            storedAddressLowByte + addendInt
                          internalNegative =
                            (storedAddressLowByteInt' < 0x00)
                          internalOverflow =
                            internalNegative
                            || (storedAddressLowByteInt' > 0xFF)
                          storedAddressLowByte' =
                            fromIntegral
                             storedAddressLowByteInt'
                            :: Word8
                          storedAddress' =
                            fromIntegral storedAddressLowByte'
                            .|. storedAddressHighByte
                      in (storedAddress',
                          internalOverflow,
                          internalNegative)
                storedAddress'' =
                  if microcodeInstructionZeroStoredAddressHighByte
                      microcodeInstruction
                    then storedAddress' .&. 0x00FF
                    else storedAddress'
                stackPointer =
                  cpu6502StateStackPointer cpuState''
                stackPointer' =
                  case microcodeInstructionStackPointerOperation
                        microcodeInstruction of
                    Nothing -> stackPointer
                    Just Increment -> stackPointer + 1
                    Just Decrement -> stackPointer - 1
                xIndexRegister =
                  cpu6502StateXIndexRegister cpuState''
                xIndexRegister' =
                  case microcodeInstructionXIndexRegisterOperation
                        microcodeInstruction of
                    Nothing -> xIndexRegister
                    Just Increment -> xIndexRegister + 1
                    Just Decrement -> xIndexRegister - 1
                yIndexRegister =
                  cpu6502StateYIndexRegister cpuState''
                yIndexRegister' =
                  case microcodeInstructionYIndexRegisterOperation
                        microcodeInstruction of
                    Nothing -> yIndexRegister
                    Just Increment -> yIndexRegister + 1
                    Just Decrement -> yIndexRegister - 1
                statusRegister =
                  cpu6502StateStatusRegister cpuState''
                accumulator =
                  cpu6502StateAccumulator cpuState''
                (accumulator', statusRegister') =
                  case microcodeInstructionAccumulatorOperation
                        microcodeInstruction of
                    Nothing -> (accumulator, statusRegister)
                    Just transformation ->
                      let oldCarry = statusTestCarry statusRegister
                          (accumulator', newCarry) =
                            transformWord8 transformation
                                           (accumulator, oldCarry)
                          statusRegister' =
                            updateStatusRegisterForValueAndCarry
                             statusRegister accumulator' newCarry
                      in (accumulator', statusRegister')
                latch =
                  cpu6502StateInternalLatch cpuState''
                (latch', statusRegister'') =
                  case microcodeInstructionLatchOperation
                        microcodeInstruction of
                    Nothing -> (latch, statusRegister')
                    Just transformation ->
                      let oldCarry = statusTestCarry statusRegister
                          (latch', newCarry) =
                            transformWord8 transformation
                                           (latch, oldCarry)
                          statusRegister'' =
                            updateStatusRegisterForValueAndCarry
                             statusRegister' latch' newCarry
                      in (latch', statusRegister'')
                statusRegister''' =
                  case microcodeInstructionStatusRegisterOperation
                        microcodeInstruction of
                    Nothing -> statusRegister''
                    Just (Set, bits) -> statusRegister'' .|. bits
                    Just (Clear, bits) -> statusRegister'' .&. complement bits
                internalOverflow' =
                  internalOverflowA' || internalOverflowB'
                internalNegative' =
                  internalNegativeA' || internalNegativeB'
                microcodeInstructionQueue'' =
                  if microcodeInstructionDecodeOperation
                      microcodeInstruction
                    then if checkForInstead microcodeInstruction
                            && programCounterHighByteWasWrong
                           then microcodeInstructionQueue'
                           else decodeOperation fetchedByte
                    else case microcodeInstructionConditional
                               microcodeInstruction of
                           Nothing -> microcodeInstructionQueue'
                           Just (condition, ifTrue, ifFalse) ->
                             if testCondition condition cpuState
                               then ifTrue
                               else ifFalse
                cpuState''' = cpuState'' {
                                  cpu6502StateProgramCounter = programCounter',
                                  cpu6502StateInternalStoredAddress =
                                    storedAddress',
                                  cpu6502StateStackPointer = stackPointer',
                                  cpu6502StateXIndexRegister = xIndexRegister',
                                  cpu6502StateYIndexRegister = yIndexRegister',
                                  cpu6502StateAccumulator = accumulator',
                                  cpu6502StateInternalLatch = latch',
                                  cpu6502StateStatusRegister =
                                    statusRegister''',
                                  cpu6502StateInternalOverflow =
                                    internalOverflow',
                                  cpu6502StateInternalNegative =
                                    internalNegative',
                                  cpu6502StateMicrocodeInstructionQueue =
                                    microcodeInstructionQueue''
                                }
                cpuState'''' = case microcodeInstructionRegisterRegisterCopy
                                     microcodeInstruction of
                                 Nothing -> cpuState'''
                                 Just (source, destination) ->
                                   computeStoreInternalRegister
                                    destination
                                    cpuState'''
                                    $ computeInternalRegister
                                       source
                                       cpuState'''
                statusRegister'''' =
                  case microcodeInstructionUpdateStatusForRegister
                        microcodeInstruction of
                    Nothing -> cpu6502StateStatusRegister cpuState''''
                    Just register ->
                      updateStatusRegisterForValue
                       (cpu6502StateStatusRegister cpuState'''')
                       $ computeInternalRegister register cpuState''''
                cpuState''''' = cpuState'''' {
                                    cpu6502StateStatusRegister =
                                      statusRegister''''
                                  }
            in (cpuState''''', outerState')
      outerState'' = putState outerState' cpuState'
  in outerState''


computeEffectiveAddress :: CPU_6502_State -> MicrocodeInstruction -> Word16
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
      addressBeforePossibleIncrement = baseAddress + addressOffset
      addressHighByte =
        fromIntegral (shiftR addressBeforePossibleIncrement 8 .&. 0xFF)
        :: Word8
      addressBeforePossibleIncrementLowByte =
        fromIntegral (addressBeforePossibleIncrement .&. 0xFF)
        :: Word8
      possibleIncrement =
        if microcodeInstructionAddressAddOne microcodeInstruction
          then 1
          else 0
      addressAfterPossibleIncrementLowByte =
        addressBeforePossibleIncrementLowByte + possibleIncrement
      addressAfterPossibleIncrement =
        shiftL (fromIntegral addressHighByte) 8
        .|. fromIntegral addressAfterPossibleIncrementLowByte
  in addressAfterPossibleIncrement


computeInternalRegister :: InternalRegister -> CPU_6502_State -> Word8
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
    Latch ->
      cpu6502StateInternalLatch cpuState


computeStoreInternalRegister
    :: InternalRegister -> CPU_6502_State -> Word8 -> CPU_6502_State
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
    Latch ->
      cpuState {
          cpu6502StateInternalLatch = byte
        }
    NoRegister ->
      cpuState


performArithmetic :: ArithmeticOperation
                  -> Word8
                  -> Word8
                  -> Word8
                  -> (Word8, Word8)
performArithmetic operation oldStatus byteA byteB =
  let (result, maybeCarry, maybeOverflow,
       maybeNegativeOverride, maybeZeroOverride) =
        case operation of
          ArithmeticIdentity ->
            (byteB, Nothing, Nothing, Nothing, Nothing)
          ArithmeticAnd ->
            (byteA .&. byteB, Nothing, Nothing, Nothing, Nothing)
          ArithmeticInclusiveOr ->
            (byteA .|. byteB, Nothing, Nothing, Nothing, Nothing)
          ArithmeticExclusiveOr ->
            (xor byteA byteB, Nothing, Nothing, Nothing, Nothing)
          ArithmeticAdd ->
            let inputCarryBit = if statusTestCarry oldStatus
                                  then 1
                                  else 0
                word16A = fromIntegral byteA :: Word16
                word16B = fromIntegral byteB :: Word16
                word16Result = word16A + word16B + inputCarryBit
                byteResult = fromIntegral word16Result
                outputCarry = word16Result > 255
                outputOverflow = ((xor byteA byteResult)
                                  .&. (xor byteB byteResult)
                                  .&. 0x80)
                                 == 0x80
            in (byteResult,
                Just outputCarry,
                Just outputOverflow,
                Nothing,
                Nothing)
          ArithmeticSubtract ->
            let inputBorrowBit = if statusTestCarry oldStatus
                                   then 0
                                   else 1
                                 :: Int
                int8A = fromIntegral byteA :: Int8
                int8B = fromIntegral byteB :: Int8
                byteResult =
                  fromIntegral (int8A - int8B - fromIntegral inputBorrowBit)
                  :: Word8
                outputBorrow = (fromIntegral byteB + inputBorrowBit)
                               > fromIntegral byteA
                outputOverflow = ((xor byteA byteB)
                                  .&. (xor byteA byteResult)
                                  .&. 0x80)
                                 == 0x80
            in (byteResult,
                Just $ not outputBorrow,
                Just outputOverflow,
                Nothing,
                Nothing)
          ArithmeticCompare ->
            let int8A = fromIntegral byteA :: Int8
                int8B = fromIntegral byteB :: Int8
                byteResult = fromIntegral (int8A - int8B) :: Word8
                outputBorrow = fromIntegral byteB > fromIntegral byteA
                negative = (byteResult .&. 0x80) == 0x80
                zero = byteResult == 0x00
            in (byteA,
                Just $ not outputBorrow,
                Nothing,
                Just negative,
                Just zero)
          ArithmeticBitCompare ->
            let result = byteA .&. byteB
                overflow = testBit byteB 6
                negative = testBit byteB 7
                zero = result == 0
            in (byteA,
                Nothing,
                Just overflow,
                Just negative,
                Just zero)
      negative = case maybeNegativeOverride of
                   Nothing -> (result .&. 0x80) == 0x80
                   Just negativeOverride -> negativeOverride
      zero = case maybeZeroOverride of
               Nothing -> result == 0x00
               Just zeroOverride -> zeroOverride
      newStatus = foldl (\status (bitIndex, bitValue) ->
                            if bitValue
                              then setBit status bitIndex
                              else clearBit status bitIndex)
                        oldStatus
                        $ concat [case maybeCarry of
                                    Nothing -> []
                                    Just carry -> [(0, carry)],
                                  case maybeOverflow of
                                    Nothing -> []
                                    Just overflow -> [(6, overflow)],
                                  [(7, negative),
                                   (1, zero)]]
  in (newStatus, result)


transformWord8 :: Transformation -> (Word8, Bool) -> (Word8, Bool)
transformWord8 transformation (byte, oldCarry) =
  let oldCarryBit = if oldCarry
                      then 1
                      else 0
      (result, newCarryBit) =
        case transformation of
          ArithmeticShiftLeft ->
            (shiftL byte 1,
             shiftR byte 7)
          LogicalShiftRight ->
            (shiftR byte 1,
             byte .&. 0x01)
          RotateLeft ->
            (shiftL byte 1 .|. oldCarryBit,
             shiftR byte 7)
          RotateRight ->
            (shiftR byte 1 .|. shiftL oldCarryBit 7,
             byte .&. 0x01)
          IncrementDecrement Increment ->
            (byte + 1,
             oldCarryBit)
          IncrementDecrement Decrement -> 
            (byte - 1,
             oldCarryBit)
      newCarry = newCarryBit == 1
  in (result, newCarry)


updateStatusRegisterForValue :: Word8 -> Word8 -> Word8
updateStatusRegisterForValue oldStatus value =
  let negative = (value .&. 0x80) == 0x80
      zero = value == 0x00
  in foldl (\status (bitIndex, bitValue) ->
              if bitValue
                then setBit status bitIndex
                else clearBit status bitIndex)
          oldStatus
          [(7, negative),
           (1, zero)]


updateStatusRegisterForValueAndCarry :: Word8 -> Word8 -> Bool -> Word8
updateStatusRegisterForValueAndCarry oldStatus value carry =
  let negative = (value .&. 0x80) == 0x80
      zero = value == 0x00
  in foldl (\status (bitIndex, bitValue) ->
              if bitValue
                then setBit status bitIndex
                else clearBit status bitIndex)
          oldStatus
          [(0, carry),
           (7, negative),
           (1, zero)]


testCondition :: Condition -> CPU_6502_State -> Bool
testCondition condition cpuState =
  let status = cpu6502StateStatusRegister cpuState
  in case condition of
       CarryClear -> not $ statusTestCarry status
       CarrySet -> statusTestCarry status
       NotEqual -> not $ statusTestZero status
       Equal -> statusTestZero status
       Plus -> not $ statusTestNegative status
       Minus -> statusTestNegative status
       OverflowClear -> not $ statusTestOverflow status
       OverflowSet -> statusTestOverflow status
       InternalOverflowSet -> cpu6502StateInternalOverflow cpuState


statusTestCarry :: Word8 -> Bool
statusTestCarry status = testBit status 0


statusTestZero :: Word8 -> Bool
statusTestZero status = testBit status 1


statusTestNegative :: Word8 -> Bool
statusTestNegative status = testBit status 7


statusTestOverflow :: Word8 -> Bool
statusTestOverflow status = testBit status 6


cpu6502DecodeInstructionMnemonicAndAddressingMode
    :: Word8 -> Maybe (InstructionMnemonic, AddressingMode)
cpu6502DecodeInstructionMnemonicAndAddressingMode opcode =
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


decodeOperation :: Word8 -> [MicrocodeInstruction]
decodeOperation opcode =
  case cpu6502DecodeInstructionMnemonicAndAddressingMode opcode of
    Nothing -> []
    Just (mnemonic, addressing) ->
      case (addressing, characterizeMnemonic mnemonic) of
        (_, StackCharacter) ->
          case mnemonic of
            BRK ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                NoRegister)
                [alsoIncrementProgramCounter],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                ProgramCounterHighByte)
                [usingAddressOffsetRegister StackPointer,
                 alsoDecrementStackPointer],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                ProgramCounterLowByte)
                [usingAddressOffsetRegister StackPointer,
                 alsoDecrementStackPointer],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                StatusRegister)
                [usingAddressOffsetRegister StackPointer,
                 settingSoftwareBFlagInStoredValue,
                 alsoDecrementStackPointer],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction (FixedAddressSource 0xFFFE)
                                                ProgramCounterLowByte)
                [],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction (FixedAddressSource 0xFFFF)
                                                ProgramCounterHighByte)
                [],
               fetchOpcodeMicrocodeInstruction]
            RTI ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                NoRegister)
                [],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                NoRegister)
                [usingAddressOffsetRegister StackPointer,
                 alsoIncrementStackPointer],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                StatusRegister)
                [usingAddressOffsetRegister StackPointer,
                 alsoIncrementStackPointer],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                ProgramCounterLowByte)
                [usingAddressOffsetRegister StackPointer,
                 alsoIncrementStackPointer],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                ProgramCounterHighByte)
                [usingAddressOffsetRegister StackPointer],
               fetchOpcodeMicrocodeInstruction]
            RTS ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                NoRegister)
                [],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                NoRegister)
                [usingAddressOffsetRegister StackPointer,
                 alsoIncrementStackPointer],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                ProgramCounterLowByte)
                [usingAddressOffsetRegister StackPointer,
                 alsoIncrementStackPointer],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                ProgramCounterHighByte)
                [usingAddressOffsetRegister StackPointer],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                NoRegister)
                [alsoIncrementProgramCounter],
               fetchOpcodeMicrocodeInstruction]
            _ | elem mnemonic [PHA, PHP] ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                NoRegister)
                [],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                $ mnemonicRegister mnemonic)
                [usingAddressOffsetRegister StackPointer,
                 if mnemonic == PHP
                   then settingSoftwareBFlagInStoredValue
                   else id,
                 alsoDecrementStackPointer],
               fetchOpcodeMicrocodeInstruction]
            _ | elem mnemonic [PLA, PLP] ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                NoRegister)
                [],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                NoRegister)
                [usingAddressOffsetRegister StackPointer,
                 alsoIncrementStackPointer],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                $ mnemonicRegister mnemonic)
                [usingAddressOffsetRegister StackPointer,
                 if mnemonic == PLP
                   then clearingBFlagInFetchedValue
                   else id,
                 if mnemonic == PLA
                   then usingArithmeticOperation ArithmeticIdentity
                   else id],
               fetchOpcodeMicrocodeInstruction]
            JSR ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                Latch)
                [alsoIncrementProgramCounter],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                NoRegister)
                [usingAddressOffsetRegister StackPointer],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                ProgramCounterHighByte)
                [usingAddressOffsetRegister StackPointer,
                 alsoDecrementStackPointer],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                ProgramCounterLowByte)
                [usingAddressOffsetRegister StackPointer,
                 alsoDecrementStackPointer],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                ProgramCounterHighByte)
                [alsoCopyLatchToRegister ProgramCounterLowByte],
               fetchOpcodeMicrocodeInstruction]
        (ImpliedAddressing, _) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                NoRegister)
                $ case mnemonic of
                    CLC -> [alsoClearStatusBits 0x01]
                    CLI -> [alsoClearStatusBits 0x04]
                    CLD -> [alsoClearStatusBits 0x08]
                    CLV -> [alsoClearStatusBits 0x40]
                    SEC -> [alsoSetStatusBits 0x01]
                    SEI -> [alsoSetStatusBits 0x04]
                    SED -> [alsoSetStatusBits 0x08]
                    DEX -> [alsoDecrementXIndexRegister,
                            alsoUpdateStatusForRegister XIndexRegister]
                    DEY -> [alsoDecrementYIndexRegister,
                            alsoUpdateStatusForRegister YIndexRegister]
                    INX -> [alsoIncrementXIndexRegister,
                            alsoUpdateStatusForRegister XIndexRegister]
                    INY -> [alsoIncrementYIndexRegister,
                            alsoUpdateStatusForRegister YIndexRegister]
                    TAX -> [alsoCopyRegisterToRegister Accumulator
                                                       XIndexRegister,
                            alsoUpdateStatusForRegister XIndexRegister]
                    TAY -> [alsoCopyRegisterToRegister Accumulator
                                                       YIndexRegister,
                            alsoUpdateStatusForRegister YIndexRegister]
                    TXA -> [alsoCopyRegisterToRegister XIndexRegister
                                                       Accumulator,
                            alsoUpdateStatusForRegister Accumulator]
                    TYA -> [alsoCopyRegisterToRegister YIndexRegister
                                                       Accumulator,
                            alsoUpdateStatusForRegister Accumulator]
                    TXS -> [alsoCopyRegisterToRegister XIndexRegister
                                                       StackPointer]
                    TSX -> [alsoCopyRegisterToRegister StackPointer
                                                       XIndexRegister,
                            alsoUpdateStatusForRegister StackPointer]
                    NOP -> [],
               fetchOpcodeMicrocodeInstruction]
        (AccumulatorAddressing, _) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                NoRegister)
                $ case mnemonic of
                    ASL -> [alsoTransformAccumulator ArithmeticShiftLeft]
                    LSR -> [alsoTransformAccumulator LogicalShiftRight]
                    ROL -> [alsoTransformAccumulator RotateLeft]
                    ROR -> [alsoTransformAccumulator RotateRight],
               fetchOpcodeMicrocodeInstruction]
        (ImmediateAddressing, _) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                $ mnemonicRegister mnemonic)
                [alsoIncrementProgramCounter,
                 usingArithmeticOperation
                  $ mnemonicArithmeticOperation mnemonic],
               fetchOpcodeMicrocodeInstruction]
        (AbsoluteAddressing, ControlCharacter) ->
          case mnemonic of
            JMP ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                Latch)
                [alsoIncrementProgramCounter],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                ProgramCounterHighByte)
                [alsoCopyLatchToRegister ProgramCounterLowByte],
               fetchOpcodeMicrocodeInstruction]
        (AbsoluteAddressing, ReadCharacter) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressHighByte)
                [alsoIncrementProgramCounter],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                $ mnemonicRegister mnemonic)
                [usingArithmeticOperation
                  $ mnemonicArithmeticOperation mnemonic],
               fetchOpcodeMicrocodeInstruction]
        (AbsoluteAddressing, ReadWriteCharacter) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressHighByte)
                [alsoIncrementProgramCounter],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [alsoTransformLatch $ mnemonicTransformation mnemonic],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               fetchOpcodeMicrocodeInstruction]
        (AbsoluteAddressing, WriteCharacter) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressHighByte)
                [alsoIncrementProgramCounter],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                $ mnemonicRegister mnemonic)
                [],
               fetchOpcodeMicrocodeInstruction]
        (ZeroPageAddressing, ReadCharacter) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                $ mnemonicRegister mnemonic)
                [usingArithmeticOperation
                  $ mnemonicArithmeticOperation mnemonic],
               fetchOpcodeMicrocodeInstruction]
        (ZeroPageAddressing, ReadWriteCharacter) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [alsoTransformLatch $ mnemonicTransformation mnemonic],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               fetchOpcodeMicrocodeInstruction]
        (ZeroPageAddressing, WriteCharacter) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                $ mnemonicRegister mnemonic)
                [],
               fetchOpcodeMicrocodeInstruction]
        (_, ReadCharacter)
          | elem addressing [ZeroPageXIndexedAddressing,
                             ZeroPageYIndexedAddressing] ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                NoRegister)
                [alsoAddRegisterToStoredAddress
                  $ mnemonicIndexRegister mnemonic,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                $ mnemonicRegister mnemonic)
                [usingArithmeticOperation
                  $ mnemonicArithmeticOperation mnemonic],
               fetchOpcodeMicrocodeInstruction]
        (_, ReadWriteCharacter)
          | elem addressing [ZeroPageXIndexedAddressing,
                             ZeroPageYIndexedAddressing] ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                NoRegister)
                [alsoAddRegisterToStoredAddress
                  $ mnemonicIndexRegister mnemonic,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [alsoTransformLatch $ mnemonicTransformation mnemonic],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               fetchOpcodeMicrocodeInstruction]
        (_, WriteCharacter)
          | elem addressing [ZeroPageXIndexedAddressing,
                             ZeroPageYIndexedAddressing] ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                NoRegister)
                [alsoAddRegisterToStoredAddress
                  $ mnemonicIndexRegister mnemonic,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                $ mnemonicRegister mnemonic)
                [],
               fetchOpcodeMicrocodeInstruction]
        (_, ReadCharacter)
          | elem addressing [AbsoluteXIndexedAddressing,
                             AbsoluteYIndexedAddressing] ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressHighByte)
                [alsoIncrementProgramCounter,
                 alsoAddRegisterToStoredAddress
                  $ mnemonicIndexRegister mnemonic,
                 usingConditional
                  InternalOverflowSet
                  [buildMicrocodeInstruction
                    (fetchValueMicrocodeInstruction StoredAddressSource
                                                    NoRegister)
                    [alsoFixStoredAddressHighByte],
                   buildMicrocodeInstruction
                    (fetchValueMicrocodeInstruction
                      StoredAddressSource
                      $ mnemonicRegister mnemonic)
                    [],
                   fetchOpcodeMicrocodeInstruction]
                  [buildMicrocodeInstruction
                    (fetchValueMicrocodeInstruction
                      StoredAddressSource
                      $ mnemonicRegister mnemonic)
                    [],
                   fetchOpcodeMicrocodeInstruction]]]
        (_, ReadWriteCharacter)
          | elem addressing [AbsoluteXIndexedAddressing,
                             AbsoluteYIndexedAddressing] ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressHighByte)
                [alsoIncrementProgramCounter,
                 alsoAddRegisterToStoredAddress
                  $ mnemonicIndexRegister mnemonic],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                NoRegister)
                [alsoFixStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [alsoTransformLatch $ mnemonicTransformation mnemonic],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               fetchOpcodeMicrocodeInstruction]
        (_, WriteCharacter)
          | elem addressing [AbsoluteXIndexedAddressing,
                             AbsoluteYIndexedAddressing] ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressHighByte)
                [alsoIncrementProgramCounter,
                 alsoAddRegisterToStoredAddress
                  $ mnemonicIndexRegister mnemonic],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                NoRegister)
                [alsoFixStoredAddressHighByte],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                $ mnemonicRegister mnemonic)
                [],
               fetchOpcodeMicrocodeInstruction]
        (RelativeAddressing, ControlCharacter) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                Latch)
                [alsoIncrementProgramCounter,
                 usingConditional
                  (mnemonicCondition mnemonic)
                  [buildMicrocodeInstruction
                    (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                    NoRegister)
                    [alsoAddLatchToProgramCounterLowByte],
                   buildMicrocodeInstruction
                    fetchOpcodeMicrocodeInstruction
                    [insteadFixProgramCounterHighByteIfNecessary],
                   fetchOpcodeMicrocodeInstruction]
                  [fetchOpcodeMicrocodeInstruction]]]
        (XIndexedIndirectAddressing, ReadCharacter) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                NoRegister)
                [alsoAddRegisterToStoredAddress XIndexRegister,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                StoredAddressHighByte)
                [usingAddressPlusOne,
                 alsoCopyLatchToRegister StoredAddressLowByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                $ mnemonicRegister mnemonic)
                [usingArithmeticOperation
                  $ mnemonicArithmeticOperation mnemonic],
               fetchOpcodeMicrocodeInstruction]
        (XIndexedIndirectAddressing, ReadWriteCharacter) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                NoRegister)
                [alsoAddRegisterToStoredAddress XIndexRegister,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                StoredAddressHighByte)
                [usingAddressPlusOne,
                 alsoCopyLatchToRegister StoredAddressLowByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [alsoTransformLatch $ mnemonicTransformation mnemonic],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               fetchOpcodeMicrocodeInstruction]
        (XIndexedIndirectAddressing, WriteCharacter) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                NoRegister)
                [alsoAddRegisterToStoredAddress XIndexRegister,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                StoredAddressHighByte)
                [usingAddressPlusOne,
                 alsoCopyLatchToRegister StoredAddressLowByte],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                $ mnemonicRegister mnemonic)
                [],
               fetchOpcodeMicrocodeInstruction]
        (IndirectYIndexedAddressing, ReadCharacter) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                StoredAddressHighByte)
                [usingAddressPlusOne,
                 alsoCopyLatchToRegister StoredAddressLowByte,
                 alsoAddRegisterToStoredAddress YIndexRegister,
                 usingConditional
                  InternalOverflowSet
                  [buildMicrocodeInstruction
                    (fetchValueMicrocodeInstruction StoredAddressSource
                                                    NoRegister)
                    [alsoFixStoredAddressHighByte],
                   buildMicrocodeInstruction
                    (fetchValueMicrocodeInstruction
                      StoredAddressSource
                      $ mnemonicRegister mnemonic)
                    [usingArithmeticOperation
                      $ mnemonicArithmeticOperation mnemonic],
                   fetchOpcodeMicrocodeInstruction]
                  [buildMicrocodeInstruction
                    (fetchValueMicrocodeInstruction
                      StoredAddressSource
                      $ mnemonicRegister mnemonic)
                    [usingArithmeticOperation
                      $ mnemonicArithmeticOperation mnemonic],
                   fetchOpcodeMicrocodeInstruction]]]
        (IndirectYIndexedAddressing, ReadWriteCharacter) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                StoredAddressHighByte)
                [usingAddressPlusOne,
                 alsoCopyLatchToRegister StoredAddressLowByte,
                 alsoAddRegisterToStoredAddress YIndexRegister],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                NoRegister)
                [alsoFixStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [alsoTransformLatch $ mnemonicTransformation mnemonic],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               fetchOpcodeMicrocodeInstruction]
        (IndirectYIndexedAddressing, WriteCharacter) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter,
                 alsoZeroStoredAddressHighByte],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                StoredAddressHighByte)
                [usingAddressPlusOne,
                 alsoCopyLatchToRegister StoredAddressLowByte,
                 alsoAddRegisterToStoredAddress YIndexRegister],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                NoRegister)
                [alsoFixStoredAddressHighByte],
               buildMicrocodeInstruction
                (storeValueMicrocodeInstruction StoredAddressSource
                                                $ mnemonicRegister mnemonic)
                [],
               fetchOpcodeMicrocodeInstruction]
        (AbsoluteIndirectAddressing, ControlCharacter) ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressLowByte)
                [alsoIncrementProgramCounter],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                StoredAddressHighByte)
                [alsoIncrementProgramCounter],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                Latch)
                [],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction StoredAddressSource
                                                ProgramCounterHighByte)
                [usingAddressPlusOne,
                 alsoCopyLatchToRegister ProgramCounterLowByte],
               fetchOpcodeMicrocodeInstruction]


characterizeMnemonic :: InstructionMnemonic -> InstructionCharacter
characterizeMnemonic mnemonic =
  case mnemonic of
    BRK -> StackCharacter
    RTI -> StackCharacter
    RTS -> StackCharacter
    PHA -> StackCharacter
    PHP -> StackCharacter
    PLA -> StackCharacter
    PLP -> StackCharacter
    JSR -> StackCharacter
    JMP -> ControlCharacter
    BCC -> ControlCharacter
    BCS -> ControlCharacter
    BEQ -> ControlCharacter
    BMI -> ControlCharacter
    BNE -> ControlCharacter
    BPL -> ControlCharacter
    BVC -> ControlCharacter
    BVS -> ControlCharacter
    CLC -> RegisterCharacter
    CLD -> RegisterCharacter
    CLI -> RegisterCharacter
    CLV -> RegisterCharacter
    SEC -> RegisterCharacter
    SED -> RegisterCharacter
    SEI -> RegisterCharacter
    DEX -> RegisterCharacter
    DEY -> RegisterCharacter
    INX -> RegisterCharacter
    INY -> RegisterCharacter
    TAX -> RegisterCharacter
    TAY -> RegisterCharacter
    TSX -> RegisterCharacter
    TXA -> RegisterCharacter
    TXS -> RegisterCharacter
    TYA -> RegisterCharacter
    LDA -> ReadCharacter
    LDX -> ReadCharacter
    LDY -> ReadCharacter
    EOR -> ReadCharacter
    AND -> ReadCharacter
    ORA -> ReadCharacter
    ADC -> ReadCharacter
    SBC -> ReadCharacter
    CMP -> ReadCharacter
    CPX -> ReadCharacter
    CPY -> ReadCharacter
    BIT -> ReadCharacter
    NOP -> ReadCharacter
    ASL -> ReadWriteCharacter
    LSR -> ReadWriteCharacter
    ROL -> ReadWriteCharacter
    ROR -> ReadWriteCharacter
    INC -> ReadWriteCharacter
    DEC -> ReadWriteCharacter
    STA -> WriteCharacter
    STX -> WriteCharacter
    STY -> WriteCharacter


mnemonicRegister
    :: InstructionMnemonic -> InternalRegister
mnemonicRegister mnemonic =
  case mnemonic of
    PHA -> Accumulator
    PHP -> StatusRegister
    PLA -> Accumulator
    PLP -> StatusRegister
    AND -> Accumulator
    ORA -> Accumulator
    EOR -> Accumulator
    ADC -> Accumulator
    SBC -> Accumulator
    LDA -> Accumulator
    LDX -> XIndexRegister
    LDY -> YIndexRegister
    STA -> Accumulator
    STX -> XIndexRegister
    STY -> YIndexRegister
    CMP -> Accumulator
    CPX -> XIndexRegister
    CPY -> YIndexRegister
    BIT -> Accumulator
    NOP -> NoRegister
    ASL -> Latch
    LSR -> Latch
    ROL -> Latch
    ROR -> Latch
    INC -> Latch
    DEC -> Latch


mnemonicIndexRegister
    :: InstructionMnemonic -> InternalRegister
mnemonicIndexRegister mnemonic =
  case mnemonic of
    AND -> XIndexRegister
    ORA -> XIndexRegister
    EOR -> XIndexRegister
    ADC -> XIndexRegister
    SBC -> XIndexRegister
    LDA -> XIndexRegister
    LDX -> YIndexRegister
    LDY -> XIndexRegister
    STA -> XIndexRegister
    STX -> YIndexRegister
    STY -> XIndexRegister
    CMP -> XIndexRegister
    ASL -> XIndexRegister
    LSR -> XIndexRegister
    ROL -> XIndexRegister
    ROR -> XIndexRegister
    INC -> XIndexRegister
    DEC -> XIndexRegister


mnemonicArithmeticOperation
     :: InstructionMnemonic -> ArithmeticOperation
mnemonicArithmeticOperation mnemonic =
  case mnemonic of
    AND -> ArithmeticAnd
    ORA -> ArithmeticInclusiveOr
    EOR -> ArithmeticExclusiveOr
    ADC -> ArithmeticAdd
    SBC -> ArithmeticSubtract
    LDA -> ArithmeticIdentity
    LDX -> ArithmeticIdentity
    LDY -> ArithmeticIdentity
    CMP -> ArithmeticCompare
    CPX -> ArithmeticCompare
    CPY -> ArithmeticCompare
    BIT -> ArithmeticBitCompare


mnemonicTransformation :: InstructionMnemonic -> Transformation
mnemonicTransformation mnemonic =
  case mnemonic of
    ASL -> ArithmeticShiftLeft
    LSR -> LogicalShiftRight
    ROL -> RotateLeft
    ROR -> RotateRight
    INC -> IncrementDecrement Increment
    DEC -> IncrementDecrement Decrement


mnemonicCondition
    :: InstructionMnemonic -> Condition
mnemonicCondition mnemonic =
  case mnemonic of
    BCC -> CarryClear
    BCS -> CarrySet
    BNE -> NotEqual
    BEQ -> Equal
    BPL -> Plus
    BMI -> Minus
    BVC -> OverflowClear
    BVS -> OverflowSet


buildMicrocodeInstruction :: MicrocodeInstruction
                          -> [MicrocodeInstruction -> MicrocodeInstruction]
                          -> MicrocodeInstruction
buildMicrocodeInstruction base steps = (foldr ($)) base (reverse steps)


templateMicrocodeInstruction :: MicrocodeInstruction
templateMicrocodeInstruction =
  MicrocodeInstruction {
      microcodeInstructionConditional = Nothing,
      microcodeInstructionInsteadFixProgramCounterHighByteIfNecessary = False,
      microcodeInstructionRegister = Nothing,
      microcodeInstructionRegisterFromLatch = Nothing,
      microcodeInstructionAddLatchToProgramCounterLowByte = False,
      microcodeInstructionAddressSource = undefined,
      microcodeInstructionAddressOffset = Nothing,
      microcodeInstructionAddressAddOne = False,
      microcodeInstructionSettingStoredValueBits = Nothing,
      microcodeInstructionClearingFetchedValueBits = Nothing,
      microcodeInstructionReadWrite = undefined,
      microcodeInstructionArithmeticOperation = Nothing,
      microcodeInstructionDecodeOperation = False,
      microcodeInstructionIncrementProgramCounter = False,
      microcodeInstructionZeroStoredAddressHighByte = False,
      microcodeInstructionAddRegisterToStoredAddress = Nothing,
      microcodeInstructionFixStoredAddressHighByte = False,
      microcodeInstructionStackPointerOperation = Nothing,
      microcodeInstructionXIndexRegisterOperation = Nothing,
      microcodeInstructionYIndexRegisterOperation = Nothing,
      microcodeInstructionStatusRegisterOperation = Nothing,
      microcodeInstructionAccumulatorOperation = Nothing,
      microcodeInstructionLatchOperation = Nothing,
      microcodeInstructionRegisterRegisterCopy = Nothing,
      microcodeInstructionUpdateStatusForRegister = Nothing
    }


fetchOpcodeMicrocodeInstruction :: MicrocodeInstruction
fetchOpcodeMicrocodeInstruction =
  templateMicrocodeInstruction {
      microcodeInstructionAddressSource = ProgramCounterAddressSource,
      microcodeInstructionReadWrite = Read,
      microcodeInstructionArithmeticOperation = Nothing,
      microcodeInstructionDecodeOperation = True,
      microcodeInstructionIncrementProgramCounter = True
    }


fetchValueMicrocodeInstruction
    :: AddressSource -> InternalRegister -> MicrocodeInstruction
fetchValueMicrocodeInstruction addressSource register =
  templateMicrocodeInstruction {
      microcodeInstructionRegister = Just register,
      microcodeInstructionAddressSource = addressSource,
      microcodeInstructionReadWrite = Read
    }


storeValueMicrocodeInstruction
    :: AddressSource -> InternalRegister -> MicrocodeInstruction
storeValueMicrocodeInstruction addressSource register =
  templateMicrocodeInstruction {
      microcodeInstructionRegister = Just register,
      microcodeInstructionAddressSource = addressSource,
      microcodeInstructionReadWrite = Write
    }


stubMicrocodeInstruction :: MicrocodeInstruction
stubMicrocodeInstruction =
  templateMicrocodeInstruction {
      microcodeInstructionAddressSource = FixedAddressSource 0x0000,
      microcodeInstructionReadWrite = Read
    }


alsoIncrementProgramCounter :: MicrocodeInstruction -> MicrocodeInstruction
alsoIncrementProgramCounter microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionIncrementProgramCounter = True
    }


alsoZeroStoredAddressHighByte :: MicrocodeInstruction -> MicrocodeInstruction
alsoZeroStoredAddressHighByte microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionZeroStoredAddressHighByte = True
    }


alsoAddRegisterToStoredAddress
    :: InternalRegister -> MicrocodeInstruction -> MicrocodeInstruction
alsoAddRegisterToStoredAddress register microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionAddRegisterToStoredAddress = Just register
    }


alsoFixStoredAddressHighByte
    :: MicrocodeInstruction -> MicrocodeInstruction
alsoFixStoredAddressHighByte microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionFixStoredAddressHighByte = True
    }


alsoIncrementStackPointer :: MicrocodeInstruction -> MicrocodeInstruction
alsoIncrementStackPointer microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionStackPointerOperation = Just Increment
    }


alsoDecrementStackPointer :: MicrocodeInstruction -> MicrocodeInstruction
alsoDecrementStackPointer microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionStackPointerOperation = Just Decrement
    }


alsoIncrementXIndexRegister :: MicrocodeInstruction -> MicrocodeInstruction
alsoIncrementXIndexRegister microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionXIndexRegisterOperation = Just Increment
    }


alsoDecrementXIndexRegister :: MicrocodeInstruction -> MicrocodeInstruction
alsoDecrementXIndexRegister microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionXIndexRegisterOperation = Just Decrement
    }


alsoIncrementYIndexRegister :: MicrocodeInstruction -> MicrocodeInstruction
alsoIncrementYIndexRegister microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionYIndexRegisterOperation = Just Increment
    }


alsoDecrementYIndexRegister :: MicrocodeInstruction -> MicrocodeInstruction
alsoDecrementYIndexRegister microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionYIndexRegisterOperation = Just Decrement
    }


alsoCopyLatchToRegister
    :: InternalRegister -> MicrocodeInstruction -> MicrocodeInstruction
alsoCopyLatchToRegister register microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionRegisterFromLatch = Just register
    }


alsoAddLatchToProgramCounterLowByte
    :: MicrocodeInstruction -> MicrocodeInstruction
alsoAddLatchToProgramCounterLowByte microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionAddLatchToProgramCounterLowByte = True
    }


usingArithmeticOperation
    :: ArithmeticOperation -> MicrocodeInstruction -> MicrocodeInstruction
usingArithmeticOperation arithmeticOperation microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionArithmeticOperation = Just arithmeticOperation
    }


usingAddressOffsetRegister
    :: InternalRegister -> MicrocodeInstruction -> MicrocodeInstruction
usingAddressOffsetRegister register microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionAddressOffset = Just register
    }


usingAddressPlusOne
    :: MicrocodeInstruction -> MicrocodeInstruction
usingAddressPlusOne microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionAddressAddOne = True
    }


settingSoftwareBFlagInStoredValue
    :: MicrocodeInstruction -> MicrocodeInstruction
settingSoftwareBFlagInStoredValue microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionSettingStoredValueBits = Just 0x30
    }


settingHardwareBFlagInStoredValue
    :: MicrocodeInstruction -> MicrocodeInstruction
settingHardwareBFlagInStoredValue microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionSettingStoredValueBits = Just 0x20
    }


clearingBFlagInFetchedValue
    :: MicrocodeInstruction -> MicrocodeInstruction
clearingBFlagInFetchedValue microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionClearingFetchedValueBits = Just 0x10
    }


usingConditional
    :: Condition -> [MicrocodeInstruction] -> [MicrocodeInstruction]
    -> MicrocodeInstruction -> MicrocodeInstruction
usingConditional condition ifTrue ifFalse microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionConditional = Just (condition, ifTrue, ifFalse)
    }


alsoSetStatusBits
    :: Word8 -> MicrocodeInstruction -> MicrocodeInstruction
alsoSetStatusBits bits microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionStatusRegisterOperation = Just (Set, bits)
    }


alsoClearStatusBits
    :: Word8 -> MicrocodeInstruction -> MicrocodeInstruction
alsoClearStatusBits bits microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionStatusRegisterOperation = Just (Clear, bits)
    }


alsoTransformAccumulator
    :: Transformation
    -> MicrocodeInstruction
    -> MicrocodeInstruction
alsoTransformAccumulator transformation microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionAccumulatorOperation = Just transformation
    }


alsoTransformLatch
    :: Transformation
    -> MicrocodeInstruction
    -> MicrocodeInstruction
alsoTransformLatch transformation microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionLatchOperation = Just transformation
    }


alsoCopyRegisterToRegister
    :: InternalRegister
    -> InternalRegister
    -> MicrocodeInstruction
    -> MicrocodeInstruction
alsoCopyRegisterToRegister source destination microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionRegisterRegisterCopy = Just (source, destination)
    }


insteadFixProgramCounterHighByteIfNecessary
    :: MicrocodeInstruction -> MicrocodeInstruction
insteadFixProgramCounterHighByteIfNecessary microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionInsteadFixProgramCounterHighByteIfNecessary = True
    }


alsoUpdateStatusForRegister
    :: InternalRegister -> MicrocodeInstruction -> MicrocodeInstruction
alsoUpdateStatusForRegister register microcodeInstruction =
  microcodeInstruction {
      microcodeInstructionUpdateStatusForRegister = Just register
    }


cpu6502NBytes :: AddressingMode -> Int
cpu6502NBytes addressingMode =
  case addressingMode of
     AccumulatorAddressing -> 1
     ImmediateAddressing -> 2
     AbsoluteAddressing -> 3
     ZeroPageAddressing -> 2
     ZeroPageXIndexedAddressing -> 2
     ZeroPageYIndexedAddressing -> 2
     AbsoluteXIndexedAddressing -> 3
     AbsoluteYIndexedAddressing -> 3
     ImpliedAddressing -> 1
     RelativeAddressing -> 2
     XIndexedIndirectAddressing -> 2
     IndirectYIndexedAddressing -> 2
     AbsoluteIndirectAddressing -> 3


cpu6502AtInstructionStart :: CPU_6502_State -> Bool
cpu6502AtInstructionStart cpuState =
  case cpu6502StateMicrocodeInstructionQueue cpuState of
    [] -> False
    (microcodeInstruction : _) ->
      if microcodeInstructionDecodeOperation microcodeInstruction
        then if microcodeInstructionInsteadFixProgramCounterHighByteIfNecessary
                 microcodeInstruction
                && cpu6502StateInternalOverflow cpuState
              then False
              else True
        else False
