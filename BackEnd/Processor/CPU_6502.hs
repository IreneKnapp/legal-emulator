{-# LANGUAGE BangPatterns #-}
module Processor.CPU_6502
  (
   CPU_6502_State(..),
   MonadChip(..),
   powerOnState,
   cycle,
   getAtInstructionStart,
   disassembleInstruction
  )
  where

import Control.DeepSeq
import Data.Array.IArray
import Data.Bits
import Data.Int
import Data.List hiding (cycle)
import Data.Word
import Prelude hiding (cycle, Maybe(..))

import Assembly
import Data.Strict.Maybe

import Debug.Trace


data CPU_6502_State =
  CPU_6502_State {
      cpu6502StateProgramCounter :: ! Word16,
      cpu6502StateStackPointer :: ! Word8,
      cpu6502StateAccumulator :: ! Word8,
      cpu6502StateXIndexRegister :: ! Word8,
      cpu6502StateYIndexRegister :: ! Word8,
      cpu6502StateStatusRegister :: ! Word8,
      cpu6502StateInternalOverflow :: ! Bool,
      cpu6502StateInternalNegative :: ! Bool,
      cpu6502StateInternalStoredAddress :: ! Word16,
      cpu6502StateInternalLatch :: ! Word8,
      cpu6502StateMicrocodeInstructionQueue :: ! [MicrocodeInstruction],
      cpu6502StateInterruptNoticed :: ! (Maybe InterruptType),
      cpu6502StateInterruptAlreadyProcessed :: ! Bool,
      cpu6502StateNonMaskableInterruptAlreadyProcessed :: ! Bool
    }


data AddressSource
  = ProgramCounterAddressSource
  | FixedAddressSource Word16
  | StoredAddressSource
  deriving (Eq, Show)


data ReadWrite = Read | Write
               deriving (Eq, Show)


data InstructionMnemonic
  -- Basic mnemonics
  = ADC | AND | ASL | BCC | BCS | BEQ | BIT | BMI | BNE | BPL | BRK | BVC
  | BVS | CLC | CLD | CLI | CLV | CMP | CPX | CPY | DEC | DEX | DEY | EOR
  | INC | INX | INY | JMP | JSR | LDA | LDX | LDY | LSR | NOP | ORA | PHA
  | PHP | PLA | PLP | ROL | ROR | RTI | RTS | SBC | SEC | SED | SEI | STA
  | STX | STY | TAX | TAY | TSX | TXA | TXS | TYA
  -- Extended mnemonics
  | SLO | RLA | SRE | RRA | SAX | LAX | DCP | ISB
  | ANC | ASR | ARR | ANE | LXA | SBX | SHA | SHS | LAS | SHY | SHX
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
  | AccumulatorAndXIndexRegister
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
  | ArithmeticNoOperation
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


data InterruptType
  = Interrupt
  | NonMaskableInterrupt
  deriving (Eq, Show)


class (Monad m) => MonadChip m where
  debugFetchByte :: Word16 -> m Word8
  fetchByte :: Word16 -> m Word8
  storeByte :: Word16 -> Word8 -> m ()
  getIRQAsserted :: m Bool
  getNMIAsserted :: m Bool
  getProgramCounter :: m Word16
  putProgramCounter :: Word16 -> m ()
  getStackPointer :: m Word8
  putStackPointer :: Word8 -> m ()
  getAccumulator :: m Word8
  putAccumulator :: Word8 -> m ()
  getXIndexRegister :: m Word8
  putXIndexRegister :: Word8 -> m ()
  getYIndexRegister :: m Word8
  putYIndexRegister :: Word8 -> m ()
  getStatusRegister :: m Word8
  putStatusRegister :: Word8 -> m ()
  getInternalOverflow :: m Bool
  putInternalOverflow :: Bool -> m ()
  getInternalNegative :: m Bool
  putInternalNegative :: Bool -> m ()
  getInternalStoredAddress :: m Word16
  putInternalStoredAddress :: Word16 -> m ()
  getInternalLatch :: m Word8
  putInternalLatch :: Word8 -> m ()
  getMicrocodeInstructionQueue :: m [MicrocodeInstruction]
  putMicrocodeInstructionQueue :: [MicrocodeInstruction] -> m ()
  getInterruptNoticed :: m (Maybe InterruptType)
  putInterruptNoticed :: (Maybe InterruptType) -> m ()
  getInterruptAlreadyProcessed :: m Bool
  putInterruptAlreadyProcessed :: Bool -> m ()
  getNonMaskableInterruptAlreadyProcessed :: m Bool
  putNonMaskableInterruptAlreadyProcessed :: Bool -> m ()


instance NFData CPU_6502_State where
  rnf cpuState =
    (rnf $ cpu6502StateProgramCounter cpuState)
    `seq` (rnf $ cpu6502StateStackPointer cpuState)
    `seq` (rnf $ cpu6502StateAccumulator cpuState)
    `seq` (rnf $ cpu6502StateXIndexRegister cpuState)
    `seq` (rnf $ cpu6502StateYIndexRegister cpuState)
    `seq` (rnf $ cpu6502StateStatusRegister cpuState)
    `seq` (rnf $ cpu6502StateInternalOverflow cpuState)
    `seq` (rnf $ cpu6502StateInternalNegative cpuState)
    `seq` (rnf $ cpu6502StateInternalStoredAddress cpuState)
    `seq` (rnf $ cpu6502StateInternalLatch cpuState)
    `seq` (rnf $ cpu6502StateMicrocodeInstructionQueue cpuState)
    `seq` (rnf $ cpu6502StateInterruptNoticed cpuState)
    `seq` (rnf $ cpu6502StateInterruptAlreadyProcessed cpuState)
    `seq` (rnf $ cpu6502StateNonMaskableInterruptAlreadyProcessed cpuState)


instance NFData MicrocodeInstruction where
  rnf microcodeInstruction =
    (rnf $ microcodeInstructionConditional microcodeInstruction)
    `seq` (rnf
            $ microcodeInstructionInsteadFixProgramCounterHighByteIfNecessary
               microcodeInstruction)
    `seq` (rnf $ microcodeInstructionRegister microcodeInstruction)
    `seq` (rnf $ microcodeInstructionRegisterFromLatch microcodeInstruction)
    `seq` (rnf $ microcodeInstructionAddLatchToProgramCounterLowByte
                  microcodeInstruction)
    `seq` (rnf $ microcodeInstructionAddressSource microcodeInstruction)
    `seq` (rnf $ microcodeInstructionAddressOffset microcodeInstruction)
    `seq` (rnf $ microcodeInstructionAddressAddOne microcodeInstruction)
    `seq` (rnf $ microcodeInstructionSettingStoredValueBits
                  microcodeInstruction)
    `seq` (rnf $ microcodeInstructionClearingFetchedValueBits
                  microcodeInstruction)
    `seq` (rnf $ microcodeInstructionReadWrite microcodeInstruction)
    `seq` (rnf $ microcodeInstructionArithmeticOperation microcodeInstruction)
    `seq` (rnf $ microcodeInstructionDecodeOperation microcodeInstruction)
    `seq` (rnf $ microcodeInstructionIncrementProgramCounter
                  microcodeInstruction)
    `seq` (rnf $ microcodeInstructionZeroStoredAddressHighByte
                  microcodeInstruction)
    `seq` (rnf $ microcodeInstructionAddRegisterToStoredAddress
                  microcodeInstruction)
    `seq` (rnf $ microcodeInstructionFixStoredAddressHighByte
                  microcodeInstruction)
    `seq` (rnf $ microcodeInstructionStackPointerOperation microcodeInstruction)
    `seq` (rnf $ microcodeInstructionXIndexRegisterOperation
                  microcodeInstruction)
    `seq` (rnf $ microcodeInstructionYIndexRegisterOperation
                  microcodeInstruction)
    `seq` (rnf $ microcodeInstructionStatusRegisterOperation
                  microcodeInstruction)
    `seq` (rnf $ microcodeInstructionAccumulatorOperation microcodeInstruction)
    `seq` (rnf $ microcodeInstructionLatchOperation microcodeInstruction)
    `seq` (rnf $ microcodeInstructionRegisterRegisterCopy microcodeInstruction)
    `seq` (rnf $ microcodeInstructionUpdateStatusForRegister
                  microcodeInstruction)


instance NFData InternalRegister where


instance NFData InterruptType where


instance NFData Condition where


instance NFData ReadWrite where


instance NFData ArithmeticOperation where


instance NFData IncrementDecrement where


instance NFData SetClear where


instance NFData Transformation where
  rnf ArithmeticShiftLeft = ()
  rnf LogicalShiftRight = ()
  rnf RotateLeft = ()
  rnf RotateRight = ()
  rnf (IncrementDecrement incrementDecrement) =
    rnf incrementDecrement


instance NFData AddressSource where
  rnf ProgramCounterAddressSource = ()
  rnf (FixedAddressSource word) = rnf word
  rnf StoredAddressSource = ()


powerOnState :: CPU_6502_State
powerOnState =
  CPU_6502_State {
      cpu6502StateProgramCounter = 0xFFFF,
      cpu6502StateStackPointer = 0xFD,
      cpu6502StateAccumulator = 0x00,
      cpu6502StateXIndexRegister = 0x00,
      cpu6502StateYIndexRegister = 0x00,
      cpu6502StateStatusRegister = 0x04,
      cpu6502StateInternalOverflow = False,
      cpu6502StateInternalNegative = False,
      cpu6502StateInternalStoredAddress = 0x0000,
      cpu6502StateInternalLatch = 0x00,
      cpu6502StateMicrocodeInstructionQueue = powerOnMicrocode,
      cpu6502StateInterruptNoticed = Nothing,
      cpu6502StateInterruptAlreadyProcessed = False,
      cpu6502StateNonMaskableInterruptAlreadyProcessed = False
    }


cycle :: (MonadChip m) => m ()
{-# INLINE cycle #-}
cycle = do
  microcodeInstructionQueue <- getMicrocodeInstructionQueue
  let checkForSetting =
        microcodeInstructionSettingStoredValueBits
      checkForClearing =
        microcodeInstructionClearingFetchedValueBits
      checkForInstead =
        microcodeInstructionInsteadFixProgramCounterHighByteIfNecessary
      checkForAddLatch =
        microcodeInstructionAddLatchToProgramCounterLowByte
      (!maybeMicrocodeInstruction, !microcodeInstructionQueue') =
        case microcodeInstructionQueue of
          (first:rest) -> (Just first, rest)
          _ -> (Nothing, [])
  case maybeMicrocodeInstruction of
    Nothing -> return ()
    Just microcodeInstruction -> do
      effectiveAddress <- getEffectiveAddress microcodeInstruction
      fetchedByte <-
        case microcodeInstructionReadWrite microcodeInstruction of
          Read -> do
            fetchedByte <- fetchByte effectiveAddress
            let fetchedByte' =
                  case checkForClearing microcodeInstruction of
                    Nothing -> fetchedByte
                    Just bits -> fetchedByte .&. complement bits
            case microcodeInstructionRegister microcodeInstruction of
              Nothing -> return ()
              Just internalRegister -> do
                let maybeArithmetic =
                      microcodeInstructionArithmeticOperation
                       microcodeInstruction
                case maybeArithmetic of
                  Nothing -> do
                    putInternalRegister internalRegister fetchedByte'
                  Just arithmetic -> do
                    registerByte <- getInternalRegister internalRegister
                    status <- getStatusRegister
                    let (status', newByte) = performArithmetic arithmetic
                                                               status
                                                               registerByte
                                                               fetchedByte'
                    putInternalRegister internalRegister newByte
                    if internalRegister /= StatusRegister
                      then putStatusRegister status'
                      else return ()
            return fetchedByte'
          Write -> do
            storedByte' <-
              case microcodeInstructionRegister microcodeInstruction of
                Nothing -> return 0x00
                Just internalRegister -> getInternalRegister internalRegister
            let storedByte =
                  case checkForSetting microcodeInstruction of
                    Nothing -> storedByte'
                    Just bits -> storedByte' .|. bits
            storeByte effectiveAddress storedByte
            status <- getStatusRegister
            accumulator <- getAccumulator
            let maybeArithmetic = microcodeInstructionArithmeticOperation
                                   microcodeInstruction
            case maybeArithmetic of
              Nothing -> return ()
              Just arithmetic -> do
                let (status', accumulator') = performArithmetic arithmetic
                                                                status
                                                                accumulator
                                                                storedByte
                putStatusRegister status'
                putAccumulator accumulator'
            return 0x00
      case microcodeInstructionRegisterFromLatch microcodeInstruction of
        Nothing -> return ()
        Just internalRegister -> do
          value <- getInternalLatch
          putInternalRegister internalRegister value
      programCounter <- getProgramCounter
      let keepProgramCounter =
            (programCounter, False, False, False)
          incrementProgramCounter =
            (programCounter + 1, False, False, False)
          incrementProgramCounterOnePage =
            (programCounter + 0x0100, True, False, False)
          decrementProgramCounterOnePage =
            (programCounter - 0x0100, True, False, False)
      (!programCounter',
       !programCounterHighByteWasWrong,
       !internalOverflowA',
       !internalNegativeA') <- do
         if checkForInstead microcodeInstruction
           then do
             internalOverflow <- getInternalOverflow
             internalNegative <- getInternalNegative
             if internalOverflow
               then if internalNegative
                      then return decrementProgramCounterOnePage
                      else return incrementProgramCounterOnePage
               else if microcodeInstructionIncrementProgramCounter
                        microcodeInstruction
                      then return incrementProgramCounter
                      else return keepProgramCounter
           else if microcodeInstructionIncrementProgramCounter
                    microcodeInstruction
                  then return incrementProgramCounter
                  else if checkForAddLatch microcodeInstruction
                         then do
                           latch <- getInternalLatch
                           let latchInt =
                                 fromIntegral (fromIntegral latch :: Int8)
                                 :: Int
                               programCounterHigh =
                                 programCounter .&. 0xFF00
                               programCounterLowByte =
                                 fromIntegral (programCounter .&. 0x00FF)
                                 :: Word8
                               programCounterLowByteInt =
                                 fromIntegral (programCounter .&. 0x00FF)
                                 :: Int
                               programCounterLowByteInt' =
                                 programCounterLowByteInt + latchInt
                               programCounterLowByte' =
                                 fromIntegral programCounterLowByteInt'
                                 :: Word8
                               programCounter' =
                                 fromIntegral programCounterLowByte'
                                 .|. programCounterHigh
                               internalNegative =
                                 (latchInt < 0x00)
                               internalOverflow =
                                 (internalNegative
                                  && (programCounterLowByte
                                      < programCounterLowByte'))
                                 || (not internalNegative
                                     && (programCounterLowByte'
                                         < programCounterLowByte))
                           return (programCounter',
                                   False,
                                   internalOverflow,
                                   internalNegative)
                         else return keepProgramCounter
      storedAddress <- getInternalStoredAddress
      (!storedAddress',
       !internalOverflowB',
       !internalNegativeB') <- do
         if microcodeInstructionFixStoredAddressHighByte microcodeInstruction
           then do
             internalOverflow <- getInternalOverflow
             if internalOverflow
               then return (storedAddress + 0x0100, False, False)
               else return (storedAddress, False, False)
           else case microcodeInstructionAddRegisterToStoredAddress
                      microcodeInstruction of
                  Nothing -> return (storedAddress, False, False)
                  Just register -> do
                    addend <- getInternalRegister register
                    let addendInt8 = fromIntegral addend :: Int8
                        addendInt = fromIntegral addend :: Int
                        storedAddressHighByte = storedAddress .&. 0xFF00
                        storedAddressLowByte =
                          fromIntegral (storedAddress .&. 0x00FF) :: Int
                        storedAddressLowByteInt' =
                          storedAddressLowByte + addendInt
                        internalNegative = (storedAddressLowByteInt' < 0x00)
                        internalOverflow =
                          internalNegative || (storedAddressLowByteInt' > 0xFF)
                        storedAddressLowByte' =
                          fromIntegral storedAddressLowByteInt' :: Word8
                        storedAddress' =
                          fromIntegral storedAddressLowByte'
                          .|. storedAddressHighByte
                    return (storedAddress', internalOverflow, internalNegative)
      let storedAddress'' =
            if microcodeInstructionZeroStoredAddressHighByte
                microcodeInstruction
              then storedAddress' .&. 0x00FF
              else storedAddress'
      stackPointer <- getStackPointer
      let stackPointer' =
            case microcodeInstructionStackPointerOperation
                  microcodeInstruction of
              Nothing -> stackPointer
              Just Increment -> stackPointer + 1
              Just Decrement -> stackPointer - 1
      xIndexRegister <- getXIndexRegister
      let xIndexRegister' =
            case microcodeInstructionXIndexRegisterOperation
                  microcodeInstruction of
              Nothing -> xIndexRegister
              Just Increment -> xIndexRegister + 1
              Just Decrement -> xIndexRegister - 1
      yIndexRegister <- getYIndexRegister
      let yIndexRegister' =
            case microcodeInstructionYIndexRegisterOperation
                  microcodeInstruction of
              Nothing -> yIndexRegister
              Just Increment -> yIndexRegister + 1
              Just Decrement -> yIndexRegister - 1
      statusRegister <- getStatusRegister
      accumulator <- getAccumulator
      let (!accumulator', !statusRegister') =
             case microcodeInstructionAccumulatorOperation
                   microcodeInstruction of
               Nothing -> (accumulator, statusRegister)
               Just transformation ->
                 let oldCarry = statusTestCarry statusRegister
                     (!accumulator', !newCarry) =
                        transformWord8 transformation (accumulator, oldCarry)
                     statusRegister' =
                       updateStatusRegisterForValueAndCarry statusRegister
                                                            accumulator'
                                                            newCarry
                 in (accumulator', statusRegister')
      latch <- getInternalLatch
      let (!latch', !statusRegister'') =
             case microcodeInstructionLatchOperation microcodeInstruction of
               Nothing -> (latch, statusRegister')
               Just transformation ->
                 let oldCarry = statusTestCarry statusRegister
                     (!latch', !newCarry) =
                        transformWord8 transformation (latch, oldCarry)
                     statusRegister'' =
                       updateStatusRegisterForValueAndCarry statusRegister'
                                                            latch'
                                                            newCarry
                 in (latch', statusRegister'')
          statusRegister''' =
            case microcodeInstructionStatusRegisterOperation
                  microcodeInstruction of
              Nothing -> statusRegister''
              Just (Set, bits) -> statusRegister'' .|. bits
              Just (Clear, bits) -> statusRegister'' .&. complement bits
          internalOverflow' = internalOverflowA' || internalOverflowB'
          internalNegative' = internalNegativeA' || internalNegativeB'
      interruptAlreadyProcessed <- getInterruptAlreadyProcessed
      nonMaskableInterruptAlreadyProcessed <-
        getNonMaskableInterruptAlreadyProcessed
      irqAsserted <- getIRQAsserted
      nmiAsserted <- getNMIAsserted
      let interruptAlreadyProcessed' =
            interruptAlreadyProcessed && irqAsserted
          nonMaskableInterruptAlreadyProcessed' =
            nonMaskableInterruptAlreadyProcessed && nmiAsserted
      interruptNoticed <- getInterruptNoticed
      let interruptNoticed' =
            case (interruptNoticed,
                  irqAsserted && not interruptAlreadyProcessed,
                  nmiAsserted && not nonMaskableInterruptAlreadyProcessed) of
              (Just NonMaskableInterrupt, _, _) ->
                Just NonMaskableInterrupt
              (_, _, True) ->
                Just NonMaskableInterrupt
              (Just Interrupt, _, _) ->
                Just Interrupt
              (_, True, False) ->
                Just Interrupt
              (_, False, False) ->
                interruptNoticed
      putProgramCounter programCounter'
      putInternalStoredAddress storedAddress''
      putStackPointer stackPointer'
      putXIndexRegister xIndexRegister'
      putYIndexRegister yIndexRegister'
      putAccumulator accumulator'
      putInternalLatch latch'
      putStatusRegister statusRegister'''
      putInternalOverflow internalOverflow'
      putInternalNegative internalNegative'
      putInterruptNoticed interruptNoticed'
      putInterruptAlreadyProcessed interruptAlreadyProcessed'
      putNonMaskableInterruptAlreadyProcessed
       nonMaskableInterruptAlreadyProcessed'
      if microcodeInstructionDecodeOperation microcodeInstruction
        then if checkForInstead microcodeInstruction
                && programCounterHighByteWasWrong
               then do
                 putMicrocodeInstructionQueue microcodeInstructionQueue'
               else do
                 putMicrocodeInstructionQueue
                  $ case interruptNoticed' of
                      Nothing -> decodeOperation fetchedByte
                      Just Interrupt -> interruptMicrocode
                      Just NonMaskableInterrupt ->
                       nonMaskableInterruptMicrocode
                 putInterruptNoticed Nothing
        else case microcodeInstructionConditional microcodeInstruction of
               Nothing -> do
                 putMicrocodeInstructionQueue microcodeInstructionQueue'
               Just (condition, ifTrue, ifFalse) -> do
                 testResult <- testCondition condition
                 if testResult
                   then putMicrocodeInstructionQueue ifTrue
                   else putMicrocodeInstructionQueue ifFalse
      case microcodeInstructionRegisterRegisterCopy microcodeInstruction of
        Nothing -> return ()
        Just (source, destination) -> do
          value <- getInternalRegister source
          putInternalRegister destination value
      case microcodeInstructionUpdateStatusForRegister microcodeInstruction of
        Nothing -> return ()
        Just register -> do
          statusRegister <- getStatusRegister
          value <- getInternalRegister register
          let statusRegister' =
                updateStatusRegisterForValue statusRegister value
          putStatusRegister statusRegister'


getEffectiveAddress :: (MonadChip m) => MicrocodeInstruction -> m Word16
getEffectiveAddress microcodeInstruction = do
  baseAddress <-
    case microcodeInstructionAddressSource microcodeInstruction of
      ProgramCounterAddressSource -> getProgramCounter
      FixedAddressSource address -> return address
      StoredAddressSource -> getInternalStoredAddress
  addressOffset <-
    case microcodeInstructionAddressOffset microcodeInstruction of
      Nothing -> return 0
      Just internalRegister -> do
        value <- getInternalRegister internalRegister
        return $ fromIntegral value
  let addressBeforePossibleIncrement = baseAddress + addressOffset
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
  return addressAfterPossibleIncrement


getInternalRegister :: (MonadChip m) => InternalRegister -> m Word8
getInternalRegister internalRegister = do
  case internalRegister of
    ProgramCounterHighByte -> do
      programCounter <- getProgramCounter
      return $ fromIntegral $ shiftR programCounter 8 .&. 0xFF
    ProgramCounterLowByte -> do
      programCounter <- getProgramCounter
      return $ fromIntegral $ shiftR programCounter 0 .&. 0xFF
    StackPointer -> do
      getStackPointer
    Accumulator -> do
      getAccumulator
    XIndexRegister -> do
      getXIndexRegister
    YIndexRegister -> do
      getYIndexRegister
    StatusRegister -> do
      getStatusRegister
    StoredAddressHighByte -> do
      storedAddress <- getInternalStoredAddress
      return $ fromIntegral $ shiftR storedAddress 8 .&. 0xFF
    StoredAddressLowByte -> do
      storedAddress <- getInternalStoredAddress
      return $ fromIntegral $ shiftR storedAddress 0 .&. 0xFF
    Latch -> do
      getInternalLatch
    AccumulatorAndXIndexRegister -> do
      accumulator <- getAccumulator
      xIndexRegister <- getXIndexRegister
      return $ accumulator .&. xIndexRegister


putInternalRegister :: (MonadChip m) => InternalRegister -> Word8 -> m ()
putInternalRegister internalRegister byte = do
  case internalRegister of
    ProgramCounterHighByte -> do
      programCounter <- getProgramCounter
      let otherByte = fromIntegral $ shiftR programCounter 0 .&. 0xFF
          programCounter' =
            (shiftL (fromIntegral byte) 8)
            .|. (shiftL (fromIntegral otherByte) 0)
      putProgramCounter programCounter'
    ProgramCounterLowByte -> do
      programCounter <- getProgramCounter
      let otherByte = fromIntegral $ shiftR programCounter 8 .&. 0xFF
          programCounter' =
            (shiftL (fromIntegral otherByte) 8)
            .|. (shiftL (fromIntegral byte) 0)
      putProgramCounter programCounter'
    StackPointer -> do
      putStackPointer byte
    Accumulator -> do
      putAccumulator byte
    XIndexRegister -> do
      putXIndexRegister byte
    YIndexRegister -> do
      putYIndexRegister byte
    StatusRegister -> do
      putStatusRegister byte
    StoredAddressHighByte -> do
      storedAddress <- getInternalStoredAddress
      let otherByte = fromIntegral $ shiftR storedAddress 0 .&. 0xFF
          storedAddress' =
            (shiftL (fromIntegral byte) 8)
            .|. (shiftL (fromIntegral otherByte) 0)
      putInternalStoredAddress storedAddress'
    StoredAddressLowByte -> do
      storedAddress <- getInternalStoredAddress
      let otherByte = fromIntegral $ shiftR storedAddress 8 .&. 0xFF
          storedAddress' =
            (shiftL (fromIntegral otherByte) 8)
            .|. (shiftL (fromIntegral byte) 0)
      putInternalStoredAddress storedAddress'
    Latch -> do
      putInternalLatch byte
    NoRegister -> do
      return ()
    AccumulatorAndXIndexRegister -> do
      putAccumulator byte
      putXIndexRegister byte


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
            let byteResult = byteA .&. byteB
                overflow = testBit byteB 6
                negative = testBit byteB 7
                zero = byteResult == 0
            in (byteA,
                Nothing,
                Just overflow,
                Just negative,
                Just zero)
          ArithmeticNoOperation ->
            (byteA,
             Nothing,
             Nothing,
             Just $ testBit oldStatus 7,
             Just $ testBit oldStatus 1)
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


testCondition :: (MonadChip m) => Condition -> m Bool
testCondition condition = do
  status <- getStatusRegister
  case condition of
    CarryClear -> return $ not $ statusTestCarry status
    CarrySet -> return $ statusTestCarry status
    NotEqual -> return $ not $ statusTestZero status
    Equal -> return $ statusTestZero status
    Plus -> return $ not $ statusTestNegative status
    Minus -> return $ statusTestNegative status
    OverflowClear -> return $ not $ statusTestOverflow status
    OverflowSet -> return $ statusTestOverflow status
    InternalOverflowSet -> getInternalOverflow


statusTestCarry :: Word8 -> Bool
statusTestCarry status = testBit status 0


statusTestZero :: Word8 -> Bool
statusTestZero status = testBit status 1


statusTestNegative :: Word8 -> Bool
statusTestNegative status = testBit status 7


statusTestOverflow :: Word8 -> Bool
statusTestOverflow status = testBit status 6


decodeInstructionMnemonicAndAddressingMode
    :: Word8 -> Maybe (InstructionMnemonic, AddressingMode, Bool)
decodeInstructionMnemonicAndAddressingMode !opcode =
  instructionMnemonicAndAddressingModeLookupTable ! opcode


instructionMnemonicAndAddressingModeLookupTable
    :: Array Word8 (Maybe (InstructionMnemonic, AddressingMode, Bool))
instructionMnemonicAndAddressingModeLookupTable =
  array (0x00, 0xFF)
        $ map (\opcode ->
                 (opcode,
                  decodeInstructionMnemonicAndAddressingMode' opcode))
              [0x00 .. 0xFF]


decodeInstructionMnemonicAndAddressingMode'
    :: Word8 -> Maybe (InstructionMnemonic, AddressingMode, Bool)
decodeInstructionMnemonicAndAddressingMode' !opcode =
  case opcode of
    0x00 -> Just (BRK, ImpliedAddressing, False)
    0x01 -> Just (ORA, XIndexedIndirectAddressing, False)
    0x02 -> Nothing
    0x03 -> Just (SLO, XIndexedIndirectAddressing, True)
    0x04 -> Just (NOP, ZeroPageAddressing, True)
    0x05 -> Just (ORA, ZeroPageAddressing, False)
    0x06 -> Just (ASL, ZeroPageAddressing, False)
    0x07 -> Just (SLO, ZeroPageAddressing, True)
    0x08 -> Just (PHP, ImpliedAddressing, False)
    0x09 -> Just (ORA, ImmediateAddressing, False)
    0x0A -> Just (ASL, AccumulatorAddressing, False)
    0x0B -> Just (ANC, ImmediateAddressing, True)
    0x0C -> Just (NOP, AbsoluteAddressing, True)
    0x0D -> Just (ORA, AbsoluteAddressing, False)
    0x0E -> Just (ASL, AbsoluteAddressing, False)
    0x0F -> Just (SLO, AbsoluteAddressing, True)
    0x10 -> Just (BPL, RelativeAddressing, False)
    0x11 -> Just (ORA, IndirectYIndexedAddressing, False)
    0x12 -> Nothing
    0x13 -> Just (SLO, IndirectYIndexedAddressing, True)
    0x14 -> Just (NOP, ZeroPageXIndexedAddressing, True)
    0x15 -> Just (ORA, ZeroPageXIndexedAddressing, False)
    0x16 -> Just (ASL, ZeroPageXIndexedAddressing, False)
    0x17 -> Just (SLO, ZeroPageXIndexedAddressing, True)
    0x18 -> Just (CLC, ImpliedAddressing, False)
    0x19 -> Just (ORA, AbsoluteYIndexedAddressing, False)
    0x1A -> Just (NOP, ImpliedAddressing, True)
    0x1B -> Just (SLO, AbsoluteYIndexedAddressing, True)
    0x1C -> Just (NOP, AbsoluteXIndexedAddressing, True)
    0x1D -> Just (ORA, AbsoluteXIndexedAddressing, False)
    0x1E -> Just (ASL, AbsoluteXIndexedAddressing, False)
    0x1F -> Just (SLO, AbsoluteXIndexedAddressing, True)
    0x20 -> Just (JSR, AbsoluteAddressing, False)
    0x21 -> Just (AND, XIndexedIndirectAddressing, False)
    0x22 -> Nothing
    0x23 -> Just (RLA, XIndexedIndirectAddressing, True)
    0x24 -> Just (BIT, ZeroPageAddressing, False)
    0x25 -> Just (AND, ZeroPageAddressing, False)
    0x26 -> Just (ROL, ZeroPageAddressing, False)
    0x27 -> Just (RLA, ZeroPageAddressing, True)
    0x28 -> Just (PLP, ImpliedAddressing, False)
    0x29 -> Just (AND, ImmediateAddressing, False)
    0x2A -> Just (ROL, AccumulatorAddressing, False)
    0x2B -> Just (ANC, ImmediateAddressing, True)
    0x2C -> Just (BIT, AbsoluteAddressing, False)
    0x2D -> Just (AND, AbsoluteAddressing, False)
    0x2E -> Just (ROL, AbsoluteAddressing, False)
    0x2F -> Just (RLA, AbsoluteAddressing, True)
    0x30 -> Just (BMI, RelativeAddressing, False)
    0x31 -> Just (AND, IndirectYIndexedAddressing, False)
    0x32 -> Nothing
    0x33 -> Just (RLA, IndirectYIndexedAddressing, True)
    0x34 -> Just (NOP, ZeroPageXIndexedAddressing, True)
    0x35 -> Just (AND, ZeroPageXIndexedAddressing, False)
    0x36 -> Just (ROL, ZeroPageXIndexedAddressing, False)
    0x37 -> Just (RLA, ZeroPageXIndexedAddressing, True)
    0x38 -> Just (SEC, ImpliedAddressing, False)
    0x39 -> Just (AND, AbsoluteYIndexedAddressing, False)
    0x3A -> Just (NOP, ImpliedAddressing, True)
    0x3B -> Just (RLA, AbsoluteYIndexedAddressing, True)
    0x3C -> Just (NOP, AbsoluteXIndexedAddressing, True)
    0x3D -> Just (AND, AbsoluteXIndexedAddressing, False)
    0x3E -> Just (ROL, AbsoluteXIndexedAddressing, False)
    0x3F -> Just (RLA, AbsoluteXIndexedAddressing, True)
    0x40 -> Just (RTI, ImpliedAddressing, False)
    0x41 -> Just (EOR, XIndexedIndirectAddressing, False)
    0x42 -> Nothing
    0x43 -> Just (SRE, XIndexedIndirectAddressing, True)
    0x44 -> Just (NOP, ZeroPageAddressing, True)
    0x45 -> Just (EOR, ZeroPageAddressing, False)
    0x46 -> Just (LSR, ZeroPageAddressing, False)
    0x47 -> Just (SRE, ZeroPageAddressing, True)
    0x48 -> Just (PHA, ImpliedAddressing, False)
    0x49 -> Just (EOR, ImmediateAddressing, False)
    0x4A -> Just (LSR, AccumulatorAddressing, False)
    0x4B -> Just (ASR, ImmediateAddressing, True)
    0x4C -> Just (JMP, AbsoluteAddressing, False)
    0x4D -> Just (EOR, AbsoluteAddressing, False)
    0x4E -> Just (LSR, AbsoluteAddressing, False)
    0x4F -> Just (SRE, AbsoluteAddressing, True)
    0x50 -> Just (BVC, RelativeAddressing, False)
    0x51 -> Just (EOR, IndirectYIndexedAddressing, False)
    0x52 -> Nothing
    0x53 -> Just (SRE, IndirectYIndexedAddressing, True)
    0x54 -> Just (NOP, ZeroPageXIndexedAddressing, True)
    0x55 -> Just (EOR, ZeroPageXIndexedAddressing, False)
    0x56 -> Just (LSR, ZeroPageXIndexedAddressing, False)
    0x57 -> Just (SRE, ZeroPageXIndexedAddressing, True)
    0x58 -> Just (CLI, ImpliedAddressing, False)
    0x59 -> Just (EOR, AbsoluteYIndexedAddressing, False)
    0x5A -> Just (NOP, ImpliedAddressing, True)
    0x5B -> Just (SRE, AbsoluteYIndexedAddressing, True)
    0x5C -> Just (NOP, AbsoluteXIndexedAddressing, True)
    0x5D -> Just (EOR, AbsoluteXIndexedAddressing, False)
    0x5E -> Just (LSR, AbsoluteXIndexedAddressing, False)
    0x5F -> Just (SRE, AbsoluteXIndexedAddressing, True)
    0x60 -> Just (RTS, ImpliedAddressing, False)
    0x61 -> Just (ADC, XIndexedIndirectAddressing, False)
    0x62 -> Nothing
    0x63 -> Just (RRA, XIndexedIndirectAddressing, True)
    0x64 -> Just (NOP, ZeroPageAddressing, True)
    0x65 -> Just (ADC, ZeroPageAddressing, False)
    0x66 -> Just (ROR, ZeroPageAddressing, False)
    0x67 -> Just (RRA, ZeroPageAddressing, True)
    0x68 -> Just (PLA, ImpliedAddressing, False)
    0x69 -> Just (ADC, ImmediateAddressing, False)
    0x6A -> Just (ROR, AccumulatorAddressing, False)
    0x6B -> Just (ARR, ImmediateAddressing, True)
    0x6C -> Just (JMP, AbsoluteIndirectAddressing, False)
    0x6D -> Just (ADC, AbsoluteAddressing, False)
    0x6E -> Just (ROR, AbsoluteAddressing, False)
    0x6F -> Just (RRA, AbsoluteAddressing, True)
    0x70 -> Just (BVS, RelativeAddressing, False)
    0x71 -> Just (ADC, IndirectYIndexedAddressing, False)
    0x72 -> Nothing
    0x73 -> Just (RRA, IndirectYIndexedAddressing, True)
    0x74 -> Just (NOP, ZeroPageXIndexedAddressing, True)
    0x75 -> Just (ADC, ZeroPageXIndexedAddressing, False)
    0x76 -> Just (ROR, ZeroPageXIndexedAddressing, False)
    0x77 -> Just (RRA, ZeroPageXIndexedAddressing, True)
    0x78 -> Just (SEI, ImpliedAddressing, False)
    0x79 -> Just (ADC, AbsoluteYIndexedAddressing, False)
    0x7A -> Just (NOP, ImpliedAddressing, True)
    0x7B -> Just (RRA, AbsoluteYIndexedAddressing, True)
    0x7C -> Just (NOP, AbsoluteXIndexedAddressing, True)
    0x7D -> Just (ADC, AbsoluteXIndexedAddressing, False)
    0x7E -> Just (ROR, AbsoluteXIndexedAddressing, False)
    0x7F -> Just (RRA, AbsoluteXIndexedAddressing, True)
    0x80 -> Just (NOP, ImmediateAddressing, True)
    0x81 -> Just (STA, XIndexedIndirectAddressing, False)
    0x82 -> Nothing
    0x83 -> Just (SAX, XIndexedIndirectAddressing, True)
    0x84 -> Just (STY, ZeroPageAddressing, False)
    0x85 -> Just (STA, ZeroPageAddressing, False)
    0x86 -> Just (STX, ZeroPageAddressing, False)
    0x87 -> Just (SAX, ZeroPageAddressing, True)
    0x88 -> Just (DEY, ImpliedAddressing, False)
    0x89 -> Just (NOP, ImmediateAddressing, True)
    0x8A -> Just (TXA, ImpliedAddressing, False)
    0x8B -> Just (ANE, ImmediateAddressing, True)
    0x8C -> Just (STY, AbsoluteAddressing, False)
    0x8D -> Just (STA, AbsoluteAddressing, False)
    0x8E -> Just (STX, AbsoluteAddressing, False)
    0x8F -> Just (SAX, AbsoluteAddressing, True)
    0x90 -> Just (BCC, RelativeAddressing, False)
    0x91 -> Just (STA, IndirectYIndexedAddressing, False)
    0x92 -> Nothing
    0x93 -> Just (SHA, IndirectYIndexedAddressing, True)
    0x94 -> Just (STY, ZeroPageXIndexedAddressing, False)
    0x95 -> Just (STA, ZeroPageXIndexedAddressing, False)
    0x96 -> Just (STX, ZeroPageYIndexedAddressing, False)
    0x97 -> Just (SAX, ZeroPageYIndexedAddressing, True)
    0x98 -> Just (TYA, ImpliedAddressing, False)
    0x99 -> Just (STA, AbsoluteYIndexedAddressing, False)
    0x9A -> Just (TXS, ImpliedAddressing, False)
    0x9B -> Just (SHS, AbsoluteYIndexedAddressing, True)
    0x9C -> Just (SHY, AbsoluteXIndexedAddressing, True)
    0x9D -> Just (STA, AbsoluteXIndexedAddressing, False)
    0x9E -> Just (SHX, AbsoluteYIndexedAddressing, True)
    0x9F -> Just (SHA, AbsoluteYIndexedAddressing, True)
    0xA0 -> Just (LDY, ImmediateAddressing, False)
    0xA1 -> Just (LDA, XIndexedIndirectAddressing, False)
    0xA2 -> Just (LDX, ImmediateAddressing, False)
    0xA3 -> Just (LAX, XIndexedIndirectAddressing, True)
    0xA4 -> Just (LDY, ZeroPageAddressing, False)
    0xA5 -> Just (LDA, ZeroPageAddressing, False)
    0xA6 -> Just (LDX, ZeroPageAddressing, False)
    0xA7 -> Just (LAX, ZeroPageAddressing, True)
    0xA8 -> Just (TAY, ImpliedAddressing, False)
    0xA9 -> Just (LDA, ImmediateAddressing, False)
    0xAA -> Just (TAX, ImpliedAddressing, False)
    0xAB -> Just (LXA, ImmediateAddressing, True)
    0xAC -> Just (LDY, AbsoluteAddressing, False)
    0xAD -> Just (LDA, AbsoluteAddressing, False)
    0xAE -> Just (LDX, AbsoluteAddressing, False)
    0xAF -> Just (LAX, AbsoluteAddressing, True)
    0xB0 -> Just (BCS, RelativeAddressing, False)
    0xB1 -> Just (LDA, IndirectYIndexedAddressing, False)
    0xB2 -> Nothing
    0xB3 -> Just (LAX, IndirectYIndexedAddressing, True)
    0xB4 -> Just (LDY, ZeroPageXIndexedAddressing, False)
    0xB5 -> Just (LDA, ZeroPageXIndexedAddressing, False)
    0xB6 -> Just (LDX, ZeroPageYIndexedAddressing, False)
    0xB7 -> Just (LAX, ZeroPageYIndexedAddressing, True)
    0xB8 -> Just (CLV, ImpliedAddressing, False)
    0xB9 -> Just (LDA, AbsoluteYIndexedAddressing, False)
    0xBA -> Just (TSX, ImpliedAddressing, False)
    0xBB -> Just (LAS, AbsoluteYIndexedAddressing, True)
    0xBC -> Just (LDY, AbsoluteXIndexedAddressing, False)
    0xBD -> Just (LDA, AbsoluteXIndexedAddressing, False)
    0xBE -> Just (LDX, AbsoluteYIndexedAddressing, False)
    0xBF -> Just (LAX, AbsoluteYIndexedAddressing, True)
    0xC0 -> Just (CPY, ImmediateAddressing, False)
    0xC1 -> Just (CMP, XIndexedIndirectAddressing, False)
    0xC2 -> Nothing
    0xC3 -> Just (DCP, XIndexedIndirectAddressing, True)
    0xC4 -> Just (CPY, ZeroPageAddressing, False)
    0xC5 -> Just (CMP, ZeroPageAddressing, False)
    0xC6 -> Just (DEC, ZeroPageAddressing, False)
    0xC7 -> Just (DCP, ZeroPageAddressing, True)
    0xC8 -> Just (INY, ImpliedAddressing, False)
    0xC9 -> Just (CMP, ImmediateAddressing, False)
    0xCA -> Just (DEX, ImpliedAddressing, False)
    0xCB -> Just (SBX, ImmediateAddressing, True)
    0xCC -> Just (CPY, AbsoluteAddressing, False)
    0xCD -> Just (CMP, AbsoluteAddressing, False)
    0xCE -> Just (DEC, AbsoluteAddressing, False)
    0xCF -> Just (DCP, AbsoluteAddressing, True)
    0xD0 -> Just (BNE, RelativeAddressing, False)
    0xD1 -> Just (CMP, IndirectYIndexedAddressing, False)
    0xD2 -> Nothing
    0xD3 -> Just (DCP, IndirectYIndexedAddressing, True)
    0xD4 -> Just (NOP, ZeroPageXIndexedAddressing, True)
    0xD5 -> Just (CMP, ZeroPageXIndexedAddressing, False)
    0xD6 -> Just (DEC, ZeroPageXIndexedAddressing, False)
    0xD7 -> Just (DCP, ZeroPageXIndexedAddressing, True)
    0xD8 -> Just (CLD, ImpliedAddressing, False)
    0xD9 -> Just (CMP, AbsoluteYIndexedAddressing, False)
    0xDA -> Just (NOP, ImpliedAddressing, True)
    0xDB -> Just (DCP, AbsoluteYIndexedAddressing, True)
    0xDC -> Just (NOP, AbsoluteXIndexedAddressing, True)
    0xDD -> Just (CMP, AbsoluteXIndexedAddressing, False)
    0xDE -> Just (DEC, AbsoluteXIndexedAddressing, False)
    0xDF -> Just (DCP, AbsoluteXIndexedAddressing, True)
    0xE0 -> Just (CPX, ImmediateAddressing, False)
    0xE1 -> Just (SBC, XIndexedIndirectAddressing, False)
    0xE2 -> Nothing
    0xE3 -> Just (ISB, XIndexedIndirectAddressing, True)
    0xE4 -> Just (CPX, ZeroPageAddressing, False)
    0xE5 -> Just (SBC, ZeroPageAddressing, False)
    0xE6 -> Just (INC, ZeroPageAddressing, False)
    0xE7 -> Just (ISB, ZeroPageAddressing, True)
    0xE8 -> Just (INX, ImpliedAddressing, False)
    0xE9 -> Just (SBC, ImmediateAddressing, False)
    0xEA -> Just (NOP, ImpliedAddressing, False)
    0xEB -> Just (SBC, ImmediateAddressing, True)
    0xEC -> Just (CPX, AbsoluteAddressing, False)
    0xED -> Just (SBC, AbsoluteAddressing, False)
    0xEE -> Just (INC, AbsoluteAddressing, False)
    0xEF -> Just (ISB, AbsoluteAddressing, True)
    0xF0 -> Just (BEQ, RelativeAddressing, False)
    0xF1 -> Just (SBC, IndirectYIndexedAddressing, False)
    0xF2 -> Nothing
    0xF3 -> Just (ISB, IndirectYIndexedAddressing, True)
    0xF4 -> Just (NOP, ZeroPageXIndexedAddressing, True)
    0xF5 -> Just (SBC, ZeroPageXIndexedAddressing, False)
    0xF6 -> Just (INC, ZeroPageXIndexedAddressing, False)
    0xF7 -> Just (ISB, ZeroPageXIndexedAddressing, True)
    0xF8 -> Just (SED, ImpliedAddressing, False)
    0xF9 -> Just (SBC, AbsoluteYIndexedAddressing, False)
    0xFA -> Just (NOP, ImpliedAddressing, True)
    0xFB -> Just (ISB, AbsoluteYIndexedAddressing, True)
    0xFC -> Just (NOP, AbsoluteXIndexedAddressing, True)
    0xFD -> Just (SBC, AbsoluteXIndexedAddressing, False)
    0xFE -> Just (INC, AbsoluteXIndexedAddressing, False)
    0xFF -> Just (ISB, AbsoluteXIndexedAddressing, True)


decodeOperation :: Word8 -> [MicrocodeInstruction]
decodeOperation opcode = operationLookupTable ! opcode


operationLookupTable :: Array Word8 [MicrocodeInstruction]
operationLookupTable = array (0x00, 0xFF)
                             $ map (\opcode ->
                                      (opcode,
                                       decodeOperation' opcode))
                                   [0x00 .. 0xFF]


decodeOperation' :: Word8 -> [MicrocodeInstruction]
decodeOperation' opcode =
  case decodeInstructionMnemonicAndAddressingMode opcode of
    Nothing -> []
    Just (mnemonic, addressing, _) ->
      case (addressing, characterizeMnemonic mnemonic) of
        (_, StackCharacter) ->
          case mnemonic of
            BRK ->
              [buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                                NoRegister)
                [alsoIncrementProgramCounter,
                 alsoSetStatusBits 0x04],
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
                [alsoIncrementProgramCounter,
                 alsoClearStatusBits 0x04],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                NoRegister)
                [usingAddressOffsetRegister StackPointer,
                 alsoIncrementStackPointer],
               buildMicrocodeInstruction
                (fetchValueMicrocodeInstruction (FixedAddressSource 0x0100)
                                                StatusRegister)
                [usingAddressOffsetRegister StackPointer,
                 clearingBFlagInFetchedValue,
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
                  $ mnemonicArithmeticOperation mnemonic,
                 case mnemonic of
                   ASR -> alsoTransformAccumulator LogicalShiftRight
                   _ -> id],
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
                [if mnemonicPerformsArithmeticOnWrite mnemonic
                  then usingArithmeticOperation
                        $ mnemonicArithmeticOperation mnemonic
                  else id],
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
                [if mnemonicPerformsArithmeticOnWrite mnemonic
                  then usingArithmeticOperation
                        $ mnemonicArithmeticOperation mnemonic
                  else id],
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
                  $ addressingModeIndexRegister addressing,
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
                  $ addressingModeIndexRegister addressing,
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
                [if mnemonicPerformsArithmeticOnWrite mnemonic
                  then usingArithmeticOperation
                        $ mnemonicArithmeticOperation mnemonic
                  else id],
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
                  $ addressingModeIndexRegister addressing,
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
                  $ addressingModeIndexRegister addressing,
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
                      $ mnemonicArithmeticOperation mnemonic,
                     case mnemonic of
                       LAS -> alsoCopyRegisterToRegister
                               StackPointer
                               AccumulatorAndXIndexRegister
                       _ -> id],
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
                  $ addressingModeIndexRegister addressing],
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
                [if mnemonicPerformsArithmeticOnWrite mnemonic
                  then usingArithmeticOperation
                        $ mnemonicArithmeticOperation mnemonic
                  else id],
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
                  $ addressingModeIndexRegister addressing],
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
                [if mnemonicPerformsArithmeticOnWrite mnemonic
                  then usingArithmeticOperation
                        $ mnemonicArithmeticOperation mnemonic
                  else id],
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
                [if mnemonicPerformsArithmeticOnWrite mnemonic
                  then usingArithmeticOperation
                        $ mnemonicArithmeticOperation mnemonic
                  else id],
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


powerOnMicrocode :: [MicrocodeInstruction]
powerOnMicrocode =
  [buildMicrocodeInstruction
     (fetchValueMicrocodeInstruction (FixedAddressSource 0xFFFC)
                                     ProgramCounterLowByte)
     [],
   buildMicrocodeInstruction
    (fetchValueMicrocodeInstruction (FixedAddressSource 0xFFFD)
                                    ProgramCounterHighByte)
    [],
   fetchOpcodeMicrocodeInstruction]


interruptMicrocode :: [MicrocodeInstruction]
interruptMicrocode =
  [buildMicrocodeInstruction
    (fetchValueMicrocodeInstruction ProgramCounterAddressSource
                                    NoRegister)
    [alsoIncrementProgramCounter,
     alsoSetStatusBits 0x04],
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
     settingHardwareBFlagInStoredValue,
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


nonMaskableInterruptMicrocode :: [MicrocodeInstruction]
nonMaskableInterruptMicrocode =
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
     settingHardwareBFlagInStoredValue,
     alsoDecrementStackPointer],
   buildMicrocodeInstruction
    (fetchValueMicrocodeInstruction (FixedAddressSource 0xFFFA)
                                    ProgramCounterLowByte)
    [],
   buildMicrocodeInstruction
    (fetchValueMicrocodeInstruction (FixedAddressSource 0xFFFB)
                                    ProgramCounterHighByte)
    [],
   fetchOpcodeMicrocodeInstruction]


characterizeMnemonic :: InstructionMnemonic -> InstructionCharacter
characterizeMnemonic mnemonic =
  case mnemonic of
    -- Basic mnemonics
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
    -- Extended mnemonic
    ASR -> ReadCharacter
    ANE -> ReadCharacter
    SLO -> ReadWriteCharacter
    RLA -> ReadWriteCharacter
    SRE -> ReadWriteCharacter
    RRA -> ReadWriteCharacter
    SAX -> WriteCharacter
    LAX -> ReadCharacter
    LAS -> ReadCharacter
    DCP -> ReadWriteCharacter
    ISB -> ReadWriteCharacter
    LXA -> ReadCharacter
    _ -> error $ "No characterization for " ++ show mnemonic


mnemonicRegister
    :: InstructionMnemonic -> InternalRegister
mnemonicRegister mnemonic =
  case mnemonic of
    -- Basic mnemonics
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
    -- Extended mnemonics
    ASR -> Accumulator
    ANE -> Accumulator
    SLO -> Accumulator
    RLA -> Accumulator
    SRE -> Accumulator
    RRA -> Accumulator
    SAX -> AccumulatorAndXIndexRegister
    LAX -> AccumulatorAndXIndexRegister
    LAS -> StackPointer
    DCP -> Accumulator
    ISB -> Accumulator
    LXA -> Accumulator
    _ -> error $ "No register for " ++ show mnemonic


addressingModeIndexRegister
    :: AddressingMode -> InternalRegister
addressingModeIndexRegister addressing =
  case addressing of
    ZeroPageXIndexedAddressing -> XIndexRegister
    ZeroPageYIndexedAddressing -> YIndexRegister
    AbsoluteXIndexedAddressing -> XIndexRegister
    AbsoluteYIndexedAddressing -> YIndexRegister


mnemonicArithmeticOperation
     :: InstructionMnemonic -> ArithmeticOperation
mnemonicArithmeticOperation mnemonic =
  case mnemonic of
    -- Basic mnemonics
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
    NOP -> ArithmeticNoOperation
    -- Extended mnemonics
    ASR -> ArithmeticAnd
    LXA -> ArithmeticAnd
    RLA -> ArithmeticAnd
    SLO -> ArithmeticInclusiveOr
    SRE -> ArithmeticExclusiveOr
    RRA -> ArithmeticAdd
    ISB -> ArithmeticSubtract
    LAX -> ArithmeticIdentity
    DCP -> ArithmeticCompare
    _ -> error $ "No arithmetic operation for mnemonic " ++ show mnemonic


mnemonicPerformsArithmeticOnWrite :: InstructionMnemonic -> Bool
mnemonicPerformsArithmeticOnWrite mnemonic =
  case mnemonic of
    RLA -> True
    SLO -> True
    SRE -> True
    RRA -> True
    ISB -> True
    DCP -> True
    _ -> False


mnemonicTransformation :: InstructionMnemonic -> Transformation
mnemonicTransformation mnemonic =
  case mnemonic of
    -- Basic mnemonics
    ASL -> ArithmeticShiftLeft
    LSR -> LogicalShiftRight
    ROL -> RotateLeft
    ROR -> RotateRight
    INC -> IncrementDecrement Increment
    DEC -> IncrementDecrement Decrement
    -- Extended mnemonics
    SLO -> ArithmeticShiftLeft
    SRE -> LogicalShiftRight
    RLA -> RotateLeft
    RRA -> RotateRight
    ISB -> IncrementDecrement Increment
    DCP -> IncrementDecrement Decrement
    _ -> error $ "No transformation for mnemonic " ++ show mnemonic


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


addressingModeNBytes :: AddressingMode -> Int
addressingModeNBytes addressingMode =
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


getAtInstructionStart :: (MonadChip m) => m Bool
getAtInstructionStart = do
  microcodeInstructionQueue <- getMicrocodeInstructionQueue
  case microcodeInstructionQueue of
    [] -> return False
    (microcodeInstruction : _) ->
      if microcodeInstructionDecodeOperation microcodeInstruction
        then do
          if microcodeInstructionInsteadFixProgramCounterHighByteIfNecessary
                 microcodeInstruction
            then do
              internalOverflow <- getInternalOverflow
              if internalOverflow
                then return False
                else return True
            else return True
        else return False


disassembleInstruction :: (MonadChip m) => [(String, String)] -> m String
disassembleInstruction extraFields = do
  programCounter <- getProgramCounter
  opcode <- debugFetchByte programCounter
  case decodeInstructionMnemonicAndAddressingMode opcode of
    Nothing -> do
      return $ (showHexWord16 programCounter) ++ "  Invalid instruction."
    Just (instructionMnemonic, addressingMode, extended) -> do
      let nBytes = addressingModeNBytes addressingMode
      byte2 <- debugFetchByte $ programCounter + 1
      byte3 <- debugFetchByte $ programCounter + 2
      let bytes = case nBytes of
                    1 -> [opcode]
                    2 -> [opcode, byte2]
                    3 -> [opcode, byte2, byte3]
          instructionCharacter = characterizeMnemonic instructionMnemonic
          addressReport = showHexWord16 programCounter
          byteReport = rightPad (intercalate " " $ map showHexWord8 bytes)
                                8
      (lValueSubreport, maybeRValueAddress) <-
        case addressingMode of
          AccumulatorAddressing -> do
            return ("A", Nothing)
          ImmediateAddressing -> do
            return ("#$"
                    ++ showHexWord8 byte2,
                    Nothing)
          AbsoluteAddressing -> do
            return ("$"
                    ++ showHexWord8 byte3
                    ++ showHexWord8 byte2,
                    Just $ shiftL (fromIntegral byte3) 8
                           .|. fromIntegral byte2)
          ZeroPageAddressing -> do
            return ("$"
                    ++ showHexWord8 byte2,
                    Just $ fromIntegral byte2)
          ZeroPageXIndexedAddressing -> do
            x <- getXIndexRegister
            return ("$"
                    ++ showHexWord8 byte2
                    ++ ",X @ "
                    ++ showHexWord8 (byte2 + x),
                    Just $ fromIntegral $ byte2 + x)
          ZeroPageYIndexedAddressing -> do
            y <- getYIndexRegister
            return ("$"
                    ++ showHexWord8 byte2
                    ++ ",Y @ "
                    ++ showHexWord8 (byte2 + y),
                    Just $ fromIntegral $ byte2 + y)
          AbsoluteXIndexedAddressing -> do
            x <- getXIndexRegister
            let indexed = fromIntegral byte2
                          + shiftL (fromIntegral byte3) 8
                effective = indexed + fromIntegral x
            return ("$"
                    ++ showHexWord16 indexed
                    ++ ",X @ "
                    ++ showHexWord16 effective,
                    Just effective)
          AbsoluteYIndexedAddressing -> do
            y <- getYIndexRegister
            let indexed = fromIntegral byte2
                          + shiftL (fromIntegral byte3) 8
                effective = indexed + fromIntegral y
            return ("$"
                    ++ showHexWord16 indexed
                    ++ ",Y @ "
                    ++ showHexWord16 effective,
                    Just effective)
          ImpliedAddressing -> do
            return ("",
                    Nothing)
          RelativeAddressing -> do
            let programCounterInt =
                  fromIntegral programCounter + nBytes :: Int
                offsetInt =
                  fromIntegral $ (fromIntegral byte2 :: Int8) :: Int
                effectiveAddress
                  = fromIntegral $ programCounterInt + offsetInt
            return ("$" ++ showHexWord16 effectiveAddress,
                    Nothing)
          XIndexedIndirectAddressing -> do
            x <- getXIndexRegister
            effectiveLow <- debugFetchByte $ fromIntegral $ byte2 + x
            effectiveHigh <- debugFetchByte $ fromIntegral $ byte2 + x + 1
            let effective = (fromIntegral effectiveLow)
                            + shiftL (fromIntegral effectiveHigh) 8
            return ("($"
                    ++ showHexWord8 byte2
                    ++ ",X) @ "
                    ++ showHexWord8 (byte2 + x)
                    ++ " = "
                    ++ showHexWord16 effective,
                    Just effective)
          IndirectYIndexedAddressing -> do
            y <- getYIndexRegister
            indirectLow <- debugFetchByte $ fromIntegral $ byte2
            indirectHigh <- debugFetchByte $ fromIntegral $ byte2 + 1
            let indirect = fromIntegral indirectLow
                           + shiftL (fromIntegral indirectHigh) 8
                effective = indirect + fromIntegral y
            return ("($"
                    ++ showHexWord8 byte2
                    ++ "),Y = "
                    ++ showHexWord16 indirect
                    ++ " @ "
                    ++ showHexWord16 effective,
                    Just effective)
          AbsoluteIndirectAddressing -> do
            let indirect = fromIntegral byte2
                           + shiftL (fromIntegral byte3) 8
            effectiveLow <- debugFetchByte $ indirect
            effectiveHigh <- debugFetchByte $ indirect + 1
            let effective = (fromIntegral effectiveLow)
                            + shiftL (fromIntegral effectiveHigh) 8
            return ("($"
                    ++ showHexWord16 indirect
                    ++ ") = "
                    ++ showHexWord16 effective,
                    Just effective)
      let showRValueSubreport =
            isJust maybeRValueAddress
            && (((elem instructionCharacter [ReadCharacter,
                                             ReadWriteCharacter,
                                             WriteCharacter])
                 && (mnemonicRegister instructionMnemonic /= NoRegister))
                || (extended && (instructionMnemonic == NOP)))
      rValue <- case maybeRValueAddress of
                  Nothing -> return 0x00
                  Just rValueAddress -> debugFetchByte rValueAddress
      let rValueSubreport = " = " ++ showHexWord8 rValue
          disassemblyReport =
            rightPad (show instructionMnemonic
                      ++ " "
                      ++ lValueSubreport
                      ++ if showRValueSubreport
                           then rValueSubreport
                           else "")
                     30
      accumulator <- getAccumulator
      xIndexRegister <- getXIndexRegister
      yIndexRegister <- getYIndexRegister
      statusRegister <- getStatusRegister
      stackPointer <- getStackPointer
      let stateReport =
            intercalate " "
                        $ map (\(label, value) -> label ++ ":" ++ value)
                              $ [("A", showHexWord8 accumulator),
                                 ("X", showHexWord8 xIndexRegister),
                                 ("Y", showHexWord8 yIndexRegister),
                                 ("P", showHexWord8 $ statusRegister .|. 0x20),
                                 ("SP", showHexWord8 stackPointer)]
                                ++ extraFields
          report = addressReport
                   ++ "  "
                   ++ byteReport
                   ++ (if extended
                         then " *"
                         else "  ")
                   ++ disassemblyReport
                   ++ "  "
                   ++ stateReport
      return report
