{-# LANGUAGE BangPatterns, Rank2Types, TypeSynonymInstances,
             TemplateHaskell #-}
module Motherboard.NES
  (
   Mirroring(..),
   System(..),
   State(..),
   HardwareState(..),
   SoftwareState(..),
   MonadicState(..),
   runMonadicState,
   runCPU,
   runPPU,
   powerOnSoftwareState,
   cycle,
   getAtCPUCycle,
   getAboutToBeginInstruction,
   disassembleUpcomingInstruction,
  )
  where

import Control.DeepSeq
import Data.Array.Unboxed
import qualified Data.ByteString as BS
import Data.List hiding (cycle)
import Data.Word
import Prelude hiding (cycle, Maybe(..))
import qualified Control.Monad.State.Strict as State

import Assembly
import Data.Strict.Maybe
import qualified Processor.CPU_6502 as CPU
import qualified PPU.PPU_NES as PPU


data Mirroring = HorizontalMirroring
               | VerticalMirroring
               | FourScreenMirroring
               deriving (Eq, Show)


data System = PlainSystem
            | VersusUnisystem
            | PlayChoice10
            deriving (Eq, Show)


data State =
  State {
      stateHardwareState :: ! HardwareState,
      stateSoftwareState :: ! SoftwareState
      -- stateConsoleOutputBuffer :: ByteString
    }


data HardwareState =
  HardwareState {
      hardwareStateProgramReadOnlyMemory :: ! (UArray Int Word8),
      hardwareStateCharacterReadOnlyMemory :: ! (UArray Int Word8),
      hardwareStateTrainer :: ! (Maybe (UArray Int Word8)),
      hardwareStatePlayChoice10HintScreen :: ! (Maybe (UArray Int Word8)),
      hardwareStateMapperNumber :: ! Word8,
      hardwareStateMirroringType :: ! Mirroring,
      hardwareStateBatteryPresent :: ! Bool,
      hardwareStateSystem :: ! System
    }


data SoftwareState =
  SoftwareState {
      softwareStateMotherboardClockCount :: ! Int,
      softwareStateLastCPUDataBusValue :: ! Word8,
      softwareStateLastPPUDataBusValue :: ! Word8,
      softwareStateCPUState :: ! CPU.CPU_6502_State,
      softwareStatePPUState :: ! PPU.PPU_NES_State,
      softwareStateMotherboardCPUMemory :: ! (UArray Int Word8),
      softwareStateMotherboardPPUTableMemory :: ! (UArray Int Word8),
      softwareStateMotherboardPPUPaletteMemory :: ! (UArray Int Word8),
      softwareStateMotherboardPPUSpriteMemory :: ! (UArray Int Word8)
    }


data DataBus = CPUDataBus
             | PPUDataBus
             deriving (Eq, Show)


data AddressMapping = MotherboardCPUMemory
                    | MotherboardPPUTableMemory
                    | MotherboardPPUPaletteMemory
                    | MotherboardPPUSpriteMemory
                    | ProgramReadOnlyMemory
                    | CharacterReadOnlyMemory
                    | PPURegisters
                    | NoMemory
                    deriving (Eq, Show)


data Processor = CPU_6502
               | PPU_NES
               deriving (Eq, Show)


instance NFData State where
  rnf state =
    (rnf $ stateHardwareState state)
    `seq` (rnf $ stateSoftwareState state)


instance NFData HardwareState where
  rnf hardwareState =
    (rnf $ hardwareStateProgramReadOnlyMemory hardwareState)
    `seq` (rnf $ hardwareStateCharacterReadOnlyMemory hardwareState)
    `seq` (rnf $ hardwareStateTrainer hardwareState)
    `seq` (rnf $ hardwareStatePlayChoice10HintScreen hardwareState)
    `seq` (rnf $ hardwareStateMapperNumber hardwareState)
    `seq` (rnf $ hardwareStateMirroringType hardwareState)
    `seq` (rnf $ hardwareStateBatteryPresent hardwareState)
    `seq` (rnf $ hardwareStateSystem hardwareState)


instance NFData SoftwareState where
  rnf softwareState =
    (rnf $ softwareStateMotherboardClockCount softwareState)
    `seq` (rnf $ softwareStateLastCPUDataBusValue softwareState)
    `seq` (rnf $ softwareStateLastPPUDataBusValue softwareState)
    `seq` (rnf $ softwareStateCPUState softwareState)
    `seq` (rnf $ softwareStatePPUState softwareState)
    `seq` (rnf $ softwareStateMotherboardCPUMemory softwareState)
    `seq` (rnf $ softwareStateMotherboardPPUTableMemory softwareState)
    `seq` (rnf $ softwareStateMotherboardPPUPaletteMemory softwareState)
    `seq` (rnf $ softwareStateMotherboardPPUSpriteMemory softwareState)


instance NFData Mirroring where


instance NFData System where


newtype CPUMonad a = CPUMonad (State.State State a)


instance Monad CPUMonad where
  return = CPUMonad . return
  a >>= b = CPUMonad ((runCPU a) >>= (runCPU . b))


instance CPU.MonadChip CPUMonad where
  debugFetchByte address = CPUMonad $ do
    (addressMapping, localAddress) <- cpuDecodeAddress address
    debugFetch CPUDataBus addressMapping localAddress
  fetchByte address = CPUMonad $ do
    (addressMapping, localAddress) <- cpuDecodeAddress address
    fetch CPUDataBus addressMapping localAddress
  storeByte address value = CPUMonad $ do
    (addressMapping, localAddress) <- cpuDecodeAddress address
    store CPUDataBus addressMapping localAddress value
  getIRQAsserted = CPUMonad $ do
    return False
  getNMIAsserted = CPUMonad $ do
    runPPU PPU.assertingNMI
  getProgramCounter = CPUMonad $
    State.get >>= return . CPU.cpu6502StateProgramCounter
                         . stateSoftwareState
  putProgramCounter newValue = CPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateProgramCounter = newValue
                        }
                      CPU.cpu6502StateProgramCounter
                  }
  getStackPointer = CPUMonad $
    State.get >>= return . CPU.cpu6502StateStackPointer
                         . stateSoftwareState
  putStackPointer newValue = CPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateStackPointer = newValue
                        }
                      CPU.cpu6502StateStackPointer
                  }
  getAccumulator = CPUMonad $
    State.get >>= return . CPU.cpu6502StateStackPointer
                         . stateSoftwareState
  putAccumulator newValue = CPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateAccumulator = newValue
                        }
                      CPU.cpu6502StateAccumulator
                  }
  getXIndexRegister = CPUMonad $
    State.get >>= return . CPU.cpu6502StateXIndexRegister
                         . stateSoftwareState
  putXIndexRegister newValue = CPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateXIndexRegister = newValue
                        }
                      CPU.cpu6502StateXIndexRegister
                  }
  getYIndexRegister = CPUMonad $
    State.get >>= return . CPU.cpu6502StateYIndexRegister
                         . stateSoftwareState
  putYIndexRegister newValue = CPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateYIndexRegister = newValue
                        }
                      CPU.cpu6502StateYIndexRegister
                  }
  getStatusRegister = CPUMonad $
    State.get >>= return . CPU.cpu6502StateStatusRegister
                         . stateSoftwareState
  putStatusRegister  newValue = CPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateStatusRegister = newValue
                        }
                      CPU.cpu6502StateStatusRegister
                  }
  getInternalOverflow = CPUMonad $
    State.get >>= return . CPU.cpu6502StateInternalOverflow
                         . stateSoftwareState
  putInternalOverflow newValue = CPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInternalOverflow = newValue
                        }
                      CPU.cpu6502StateInternalOverflow
                  }
  getInternalNegative = CPUMonad $
    State.get >>= return . CPU.cpu6502StateInternalNegative
                         . stateSoftwareState
  putInternalNegative newValue = CPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInternalNegative = newValue
                        }
                      CPU.cpu6502StateInternalNegative
                  }
  getInternalStoredAddress = CPUMonad $ do
    State.get >>= return . CPU.cpu6502StateInternalStoredAddress
                         . stateSoftwareState
  putInternalStoredAddress newValue = CPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInternalStoredAddress = newValue
                        }
                      CPU.cpu6502StateInternalStoredAddress
                  }
  getInternalLatch = CPUMonad $
    State.get >>= return . CPU.cpu6502StateInternalLatch
                         . stateSoftwareState
  putInternalLatch newValue = CPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInternalLatch = newValue
                        }
                      CPU.cpu6502StateInternalLatch
                  }
  getMicrocodeInstructionQueue = CPUMonad $ do
    State.get >>= return . CPU.cpu6502StateMicrocodeInstructionQueue
                         . stateSoftwareState
  putMicrocodeInstructionQueue newValue = CPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateMicrocodeInstructionQueue = newValue
                        }
                      CPU.cpu6502StateMicrocodeInstructionQueue
                  }
  getInterruptNoticed = CPUMonad $
    State.get >>= return . CPU.cpu6502StateInterruptNoticed
                         . stateSoftwareState
  putInterruptNoticed newValue = CPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptNoticed = newValue
                        }
                      CPU.cpu6502StateInterruptNoticed
                  }
  getInterruptAlreadyProcessed = CPUMonad $
    State.get >>= return . CPU.cpu6502StateInterruptAlreadyProcessed
                         . stateSoftwareState
  putInterruptAlreadyProcessed newValue = CPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getNonMaskableInterruptAlreadyProcessed = CPUMonad $
    State.get >>= return . CPU.cpu6502StateNonMaskableInterruptAlreadyProcessed
                         . stateSoftwareState
  putNonMaskableInterruptAlreadyProcessed newValue = CPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateNonMaskableInterruptAlreadyProcessed
                            = newValue
                        }
                      CPU.cpu6502StateNonMaskableInterruptAlreadyProcessed
                  }


newtype PPUMonad a = PPUMonad (State.State State a)


instance PPU.MonadChip PPUMonad where
  debugFetchByte address = PPUMonad $ do
    (addressMapping, localAddress) <- cpuDecodeAddress address
    debugFetch PPUDataBus addressMapping localAddress
  fetchByte address = PPUMonad $ do
    (addressMapping, localAddress) <- ppuDecodeAddress address
    fetch PPUDataBus addressMapping localAddress
  storeByte address value = PPUMonad $ do
    (addressMapping, localAddress) <- ppuDecodeAddress address
    store PPUDataBus addressMapping localAddress value
  getTableMemory = PPUMonad $ do
    state <- State.get
    let memory = softwareStateMotherboardPPUTableMemory
                 . stateSoftwareState state
    return (\offset -> memory ! fromIntegral offset)
  getHorizontalClock = PPUMonad $
    State.get >>= return . PPU.ppuNESStateHorizontalClock
                         . stateSoftwareState
  putHorizontalClock newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getVerticalClock = PPUMonad $
    State.get >>= return . PPU.ppuNESStateVerticalClock
                         . stateSoftwareState
  putVerticalClock newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getStillPoweringUp = PPUMonad $
    State.get >>= return . PPU.ppuNESStateStillPoweringUp
                         . stateSoftwareState
  putStillPoweringUp newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getWantsToAssertNMI = PPUMonad $
    State.get >>= return . PPU.ppuNESStateWantsToAssertNMI
                         . stateSoftwareState
  putWantsToAssertNMI newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getAllowedToAssertNMI = PPUMonad $
    State.get >>= return . PPU.ppuNESStateAllowedToAssertNMI
                         . stateSoftwareState
  putAllowedToAssertNMI newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getTallSprites = PPUMonad $
    State.get >>= return . PPU.ppuNESStateTallSprites
                         . stateSoftwareState
  putTallSprites newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getPatternTableForBackground = PPUMonad $
    State.get >>= return . PPU.ppuNESStatePatternTableForBackground
                         . stateSoftwareState
  putPatternTableForBackground newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getPatternTableForSprites = PPUMonad $
    State.get >>= return . PPU.ppuNESStatePatternTableForSprites
                         . stateSoftwareState
  putPatternTableForSprites newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getAddressIncrementVertically = PPUMonad $
    State.get >>= return . PPU.ppuNESStateAddressIncrementVertically
                         . stateSoftwareState
  putAddressIncrementVertically newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getPaletteMonochrome = PPUMonad $
    State.get >>= return . PPU.ppuNESStatePaletteMonochrome
                         . stateSoftwareState
  putPaletteMonochrome newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getBackgroundClipped = PPUMonad $
    State.get >>= return . PPU.ppuNESStateBackgroundClipped
                         . stateSoftwareState
  putBackgroundClipped newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getSpritesClipped = PPUMonad $
    State.get >>= return . PPU.ppuNESStateSpritesClipped
                         . stateSoftwareState
  putSpritesClipped newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getBackgroundVisible = PPUMonad $
    State.get >>= return . PPU.ppuNESStateBackgroundVisible
                         . stateSoftwareState
  putBackgroundVisible newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getSpritesVisible = PPUMonad $
    State.get >>= return . PPU.ppuNESStateSpritesVisible
                         . stateSoftwareState
  putSpritesVisible newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getIntensifiedColor = PPUMonad $
    State.get >>= return . PPU.ppuNESStateIntensifiedColor
                         . stateSoftwareState
  putIntensifiedColor newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getWrittenOddNumberOfTimesToAddresses = PPUMonad $
    State.get >>= return . PPU.ppuNESStateWrittenOddNumberOfTimesToAddresses
                         . stateSoftwareState
  putWrittenOddNumberOfTimesToAddresses newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getPermanentAddress = PPUMonad $
    State.get >>= return . PPU.ppuNESStatePermanentAddress
                         . stateSoftwareState
  putPermanentAddress newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getTemporaryAddress = PPUMonad $
    State.get >>= return . PPU.ppuNESStateTemporaryAddress
                         . stateSoftwareState
  putTemporaryAddress newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getXOffset = PPUMonad $
    State.get >>= return . PPU.ppuNESStateXOffset
                         . stateSoftwareState
  putXOffset newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getLatestCompleteFrame = PPUMonad $
    State.get >>= return . PPU.ppuNESStateLatestCompleteFrame
                         . stateSoftwareState
  putLatestCompleteFrame newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }
  getIncompleteVideoFrameNameTableMemory = PPUMonad $
    State.get >>= return . PPU.ppuNESStateIncompleteFrameNameTableMemory
                         . stateSoftwareState
  putIncompleteVideoFrameNameTableMemory newValue = PPUMonad $ do
    oldState <- State.get
    State.put $ oldState {
                    stateSoftwareState =
                      (stateSoftwareState oldState) {
                          CPU.cpu6502StateInterruptAlreadyProcessed = newValue
                        }
                      CPU.cpu6502StateInterruptAlreadyProcessed
                  }


instance Monad PPUMonad where
  return = PPUMonad . return
  a >>= b = PPUMonad ((runPPU a) >>= (runPPU . b))


runCPU :: CPUMonad a -> MonadicState a
runCPU (CPUMonad action) = action


runPPU :: PPUMonad a -> MonadicState a
runPPU (PPUMonad action) = action


powerOnSoftwareState :: SoftwareState
powerOnSoftwareState =
  SoftwareState {
      softwareStateMotherboardClockCount = 0,
      softwareStateLastCPUDataBusValue = 0x00,
      softwareStateLastPPUDataBusValue = 0x00,
      softwareStateCPUState = CPU.powerOnState,
      softwareStatePPUState = PPU.powerOnState,
      softwareStateMotherboardCPUMemory = array (0x0000, 0x07FF)
                                                $ zip [0x0000 .. 0x07FF]
                                                      $ repeat 0x00,
      softwareStateMotherboardPPUTableMemory =
        array (0x0000, 0x07FF)
              $ zip [0x0000 .. 0x07FF]
                    $ repeat 0x00,
      softwareStateMotherboardPPUPaletteMemory =
        array (0x0000, 0x001F)
              $ zip [0x0000 .. 0x001F]
                    $ repeat 0x00,
      softwareStateMotherboardPPUSpriteMemory =
        array (0x0000, 0x00FF)
              $ zip [0x0000 .. 0x00FF]
                    $ repeat 0x00
    }


cpuDecodeAddress :: Word16 -> MonadicState (AddressMapping, Int)
cpuDecodeAddress address = do
  hardwareState <- getHardwareState
  let programReadOnlyMemory = hardwareStateProgramReadOnlyMemory hardwareState
      programReadOnlyMemoryBankSize = 0x4000
      nProgramReadOnlyMemoryBanks =
        div (1 + (snd $ bounds programReadOnlyMemory))
            programReadOnlyMemoryBankSize
      bankOffset bankIndex = bankIndex * programReadOnlyMemoryBankSize
      lowBankIndex = 0
      highBankIndex = if nProgramReadOnlyMemoryBanks < 2
                        then 0
                        else 1
  case () of
    () | address < 0x2000 -> do
          return (MotherboardCPUMemory,
                  fromIntegral $ mod address 0x0800)
       | address < 0x4000 -> do
          return (PPURegisters,
                  fromIntegral $ mod address 0x0008)
       | address < 0x8000 -> do
          return (NoMemory, 0)
       | address < 0xC000 -> do
          return (ProgramReadOnlyMemory,
                  (fromIntegral $ address - 0x8000)
                  + bankOffset lowBankIndex)
       | otherwise -> do
          return (ProgramReadOnlyMemory,
                  (fromIntegral $ address - 0xC000)
                  + bankOffset highBankIndex)


ppuDecodeAddress :: Word16 -> MonadicState (AddressMapping, Int)
ppuDecodeAddress address = do
  case mod address 0x4000 of
    address'
      | address' < 0x2000 -> do
         return (CharacterReadOnlyMemory, fromIntegral address')
      | address' < 0x3F00 -> do
          let tableIndex = div (mod (address' - 0x2000) 0x1000) 0x0400
              tableOffset = fromIntegral $ mod (address' - 0x2000) 0x0400
          case tableIndex of
            0 -> return (MotherboardPPUTableMemory,
                         0x0000 + tableOffset)
            1 -> return (MotherboardPPUTableMemory,
                         0x0400 + tableOffset)
            2 -> return (MotherboardPPUTableMemory,
                         0x0000 + tableOffset)
            3 -> return (MotherboardPPUTableMemory,
                         0x0400 + tableOffset)
      | otherwise -> do
          return (MotherboardPPUPaletteMemory,
                  fromIntegral $ mod (address' - 0x3F00) 0x20)


debugFetch :: DataBus
           -> AddressMapping
           -> Int
           -> MonadicState Word8
debugFetch dataBus addressMapping offset = do
  case addressMapping of
    MotherboardCPUMemory -> do
      memory <- getSoftwareStateMotherboardCPUMemory
      return $ memory ! offset
    MotherboardPPUTableMemory -> do
      memory <- getSoftwareStateMotherboardPPUTableMemory
      return $ memory ! offset
    MotherboardPPUPaletteMemory -> do
      memory <- getSoftwareStateMotherboardPPUPaletteMemory
      return $ memory ! offset
    MotherboardPPUSpriteMemory -> do
      memory <- getSoftwareStateMotherboardPPUSpriteMemory
      return $ memory ! offset
    ProgramReadOnlyMemory -> do
      memory <- getHardwareStateProgramReadOnlyMemory
      return $ memory ! offset
    CharacterReadOnlyMemory -> do
      memory <- getHardwareStateCharacterReadOnlyMemory
      return $ memory ! offset
    PPURegisters -> do
      return 0x00
    NoMemory -> do
      getLastDataBusValue dataBus


fetch :: DataBus
      -> AddressMapping
      -> Int
      -> MonadicState Word8
fetch dataBus addressMapping offset = do
  value <- case addressMapping of
             MotherboardCPUMemory -> do
               memory <- getSoftwareStateMotherboardCPUMemory
               return $ memory ! offset
             MotherboardPPUTableMemory -> do
               memory <- getSoftwareStateMotherboardPPUTableMemory
               return $ memory ! offset
             MotherboardPPUPaletteMemory -> do
               memory <- getSoftwareStateMotherboardPPUPaletteMemory
               return $ memory ! offset
             MotherboardPPUSpriteMemory -> do
               memory <- getSoftwareStateMotherboardPPUSpriteMemory
               return $ memory ! offset
             ProgramReadOnlyMemory -> do
               memory <- getHardwareStateProgramReadOnlyMemory
               return $ memory ! offset
             CharacterReadOnlyMemory -> do
               memory <- getHardwareStateCharacterReadOnlyMemory
               return $ memory ! offset
             PPURegisters -> do
               let !register = PPU.decodeRegister offset
                   !readable = PPU.registerReadable register
               if readable
                 then runPPU $ PPU.registerFetch register
                 else getLastDataBusValue dataBus
             NoMemory -> do
               getLastDataBusValue dataBus
  putLastDataBusValue dataBus value
  return value


store :: DataBus
      -> AddressMapping
      -> Int
      -> Word8
      -> MonadicState ()
store dataBus addressMapping offset value = do
  case addressMapping of
    MotherboardCPUMemory -> do
      memory <- getSoftwareStateMotherboardCPUMemory
      let memory' = memory // [(offset, value)]
      putSoftwareStateMotherboardCPUMemory memory'
    MotherboardPPUTableMemory -> do
      memory <- getSoftwareStateMotherboardPPUTableMemory
      let memory' = memory // [(offset, value)]
      putSoftwareStateMotherboardPPUTableMemory memory'
    MotherboardPPUPaletteMemory -> do
      memory <- getSoftwareStateMotherboardPPUPaletteMemory
      let memory' = memory // [(offset, value)]
      putSoftwareStateMotherboardPPUPaletteMemory memory'
    MotherboardPPUSpriteMemory -> do
      memory <- getSoftwareStateMotherboardPPUSpriteMemory
      let memory' = memory // [(offset, value)]
      putSoftwareStateMotherboardPPUSpriteMemory memory'
    ProgramReadOnlyMemory -> return ()
    CharacterReadOnlyMemory -> return ()
    PPURegisters -> do
      let !register = PPU.decodeRegister offset
          !writeable = PPU.registerWriteable register
      if writeable
        then runPPU $ PPU.registerStore register value
        else return ()
    NoMemory -> return ()
  putLastDataBusValue dataBus value


getLastDataBusValue :: DataBus -> MonadicState Word8
getLastDataBusValue dataBus = do
  case dataBus of
    CPUDataBus -> getSoftwareStateLastCPUDataBusValue
    PPUDataBus -> getSoftwareStateLastPPUDataBusValue


putLastDataBusValue :: DataBus -> Word8 -> MonadicState ()
putLastDataBusValue dataBus value = do
  case dataBus of
    CPUDataBus -> putSoftwareStateLastCPUDataBusValue value
    PPUDataBus -> putSoftwareStateLastPPUDataBusValue value


cycle :: MonadicState ()
{-# INLINE cycle #-}
cycle = do
  clockCount <- getSoftwareStateMotherboardClockCount
  mapM_ (\(divisor, chip) -> do
           if mod clockCount divisor == 0
             then case chip of
               CPU_6502 -> runCPU CPU.cycle
               PPU_NES -> runPPU PPU.cycle
             else return ())
        [(4, PPU_NES),
         (12, CPU_6502)]
  let clockCount' = mod (clockCount + 1) 12
  putSoftwareStateMotherboardClockCount clockCount'


getAtCPUCycle :: MonadicState Bool
getAtCPUCycle = do
  clockCount <- getSoftwareStateMotherboardClockCount
  return $ mod clockCount 12 == 0


getAboutToBeginInstruction :: MonadicState Bool
getAboutToBeginInstruction = do
  atCPUCycle <- getAtCPUCycle
  atInstructionStart <- runCPU CPU.getAtInstructionStart
  return $ atCPUCycle && atInstructionStart


disassembleUpcomingInstruction :: MonadicState String
disassembleUpcomingInstruction = do
  horizontalClock <- getSoftwareStatePPUStateHorizontalClock
  verticalClock <- getSoftwareStatePPUStateVerticalClock
  latestCompleteFrame <- getSoftwareStatePPUStateLatestCompleteFrame
  runCPU $ CPU.disassembleInstruction
             [("CYC", leftPad (show horizontalClock) 3),
              ("SL", show verticalClock),
              ("F", case latestCompleteFrame of
                      Nothing -> "no"
                      Just _ -> "yes")]
