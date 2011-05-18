{-# LANGUAGE BangPatterns, Rank2Types, TypeSynonymInstances,
             TemplateHaskell #-}
module Motherboard.NES
  (
   Mirroring(..),
   System(..),
   State(..),
   HardwareState(..),
   SoftwareState(..),
   runMonadicState,
   getState,
   putState,
   getSoftwareState,
   putSoftwareState,
   getHardwareState,
   putHardwareState,
   powerOnSoftwareState,
   cpuDecodeAddress,
   ppuDecodeAddress,
   debugFetch,
   fetch,
   store,
   cycle,
   getAtCPUCycle,
   getAboutToBeginInstruction,
   disassembleUpcomingInstruction
  )
  where

import Control.DeepSeq
import Data.Array.Unboxed
import qualified Data.ByteString as BS
import Data.List hiding (cycle)
import Data.Word
import Prelude hiding (cycle, Maybe(..))

import Assembly
import Data.FlattenedRecords
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


$(defineFlattenedRecord ''State)


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
      softwareState <- getSoftwareState
      let memory = softwareStateMotherboardCPUMemory softwareState
      return $ memory ! offset
    MotherboardPPUTableMemory -> do
      softwareState <- getSoftwareState
      let memory = softwareStateMotherboardPPUTableMemory softwareState
      return $ memory ! offset
    MotherboardPPUPaletteMemory -> do
      softwareState <- getSoftwareState
      let memory = softwareStateMotherboardPPUPaletteMemory softwareState
      return $ memory ! offset
    MotherboardPPUSpriteMemory -> do
      softwareState <- getSoftwareState
      let memory = softwareStateMotherboardPPUSpriteMemory softwareState
      return $ memory ! offset
    ProgramReadOnlyMemory -> do
      hardwareState <- getHardwareState
      let memory = hardwareStateProgramReadOnlyMemory hardwareState
      return $ memory ! offset
    CharacterReadOnlyMemory -> do
      hardwareState <- getHardwareState
      let memory = hardwareStateCharacterReadOnlyMemory hardwareState
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
               softwareState <- getSoftwareState
               let !memory = softwareStateMotherboardCPUMemory softwareState
                   !value = memory ! offset
               return value
             MotherboardPPUTableMemory -> do
               softwareState <- getSoftwareState
               let !memory = softwareStateMotherboardPPUTableMemory
                              softwareState
                   !value = memory ! offset
               return value
             MotherboardPPUPaletteMemory -> do
               softwareState <- getSoftwareState
               let !memory = softwareStateMotherboardPPUPaletteMemory
                              softwareState
                   !value = memory ! offset
               return value
             MotherboardPPUSpriteMemory -> do
               softwareState <- getSoftwareState
               let !memory = softwareStateMotherboardPPUSpriteMemory
                              softwareState
                   !value = memory ! offset
               return value
             ProgramReadOnlyMemory -> do
               hardwareState <- getHardwareState
               let !memory = hardwareStateProgramReadOnlyMemory hardwareState
                   !value = memory ! offset
               return value
             CharacterReadOnlyMemory -> do
               hardwareState <- getHardwareState
               let !memory = hardwareStateCharacterReadOnlyMemory hardwareState
                   !value = memory ! offset
               return value
             PPURegisters -> do
               let !register = PPU.decodeRegister offset
                   !readable = PPU.registerReadable register
               if readable
                 then PPU.registerFetch ppuCallbacks register
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
      softwareState <- getSoftwareState
      let !memory = softwareStateMotherboardCPUMemory softwareState
          !memory' = memory // [(offset, value)]
          !softwareState' =
             softwareState {
                 softwareStateMotherboardCPUMemory = memory'
               }
      putSoftwareState softwareState'
    MotherboardPPUTableMemory -> do
      softwareState <- getSoftwareState
      let !memory = softwareStateMotherboardPPUTableMemory softwareState
          !memory' = memory // [(offset, value)]
          !softwareState' =
             softwareState {
                 softwareStateMotherboardPPUTableMemory = memory'
               }
      putSoftwareState softwareState'
    MotherboardPPUPaletteMemory -> do
      softwareState <- getSoftwareState
      let !memory = softwareStateMotherboardPPUPaletteMemory softwareState
          !memory' = memory // [(offset, value)]
          !softwareState' =
             softwareState {
                 softwareStateMotherboardPPUPaletteMemory = memory'
               }
      putSoftwareState softwareState'
    MotherboardPPUSpriteMemory -> do
      softwareState <- getSoftwareState
      let !memory = softwareStateMotherboardPPUSpriteMemory softwareState
          !memory' = memory // [(offset, value)]
          !softwareState' =
             softwareState {
                 softwareStateMotherboardPPUSpriteMemory = memory'
               }
      putSoftwareState softwareState'
    ProgramReadOnlyMemory -> return ()
    CharacterReadOnlyMemory -> return ()
    PPURegisters -> do
      let !register = PPU.decodeRegister offset
          !writeable = PPU.registerWriteable register
      if writeable
        then PPU.registerStore ppuCallbacks register value
        else return ()
    NoMemory -> return ()
  putLastDataBusValue dataBus value


getLastDataBusValue :: DataBus -> MonadicState Word8
getLastDataBusValue dataBus = do
  softwareState <- getSoftwareState
  case dataBus of
    CPUDataBus -> return $ softwareStateLastCPUDataBusValue softwareState
    PPUDataBus -> return $ softwareStateLastPPUDataBusValue softwareState


putLastDataBusValue :: DataBus -> Word8 -> MonadicState ()
putLastDataBusValue dataBus value = do
  softwareState <- getSoftwareState
  let softwareState' =
        case dataBus of
          CPUDataBus ->
            softwareState {
                softwareStateLastCPUDataBusValue = value
              }
          PPUDataBus ->
            softwareState {
                softwareStateLastPPUDataBusValue = value
              }
  putSoftwareState softwareState'


cpuCallbacks :: ((Word16 -> MonadicState Word8),
                 (Word16 -> Word8 -> MonadicState ()),
                 (MonadicState Bool),
                 (MonadicState Bool),
                 (MonadicState CPU.CPU_6502_State),
                 (CPU.CPU_6502_State -> MonadicState ()))
cpuCallbacks = ((\address -> do
                   (addressMapping, localAddress) <- cpuDecodeAddress address
                   fetch CPUDataBus addressMapping localAddress),
                (\address value -> do
                   (addressMapping, localAddress) <- cpuDecodeAddress address
                   store CPUDataBus addressMapping localAddress value),
                (return False),
                (do
                   softwareState <- getSoftwareState
                   let ppuState = softwareStatePPUState softwareState
                       nmiAsserted = PPU.assertingNMI ppuState
                   return nmiAsserted),
                (do
                   softwareState <- getSoftwareState
                   return $ softwareStateCPUState softwareState),
                (\cpuState -> do
                   softwareState <- getSoftwareState
                   let softwareState' =
                         softwareState {
                             softwareStateCPUState = cpuState
                           }
                   putSoftwareState softwareState'))


ppuCallbacks :: ((Word16 -> MonadicState Word8),
                 (Word16 -> Word8 -> MonadicState ()),
                 (MonadicState (Word16 -> Word8)),
                 (MonadicState PPU.PPU_NES_State),
                 (PPU.PPU_NES_State -> MonadicState ()))
ppuCallbacks = ((\address -> do
                   (addressMapping, localAddress) <- ppuDecodeAddress address
                   fetch PPUDataBus addressMapping localAddress),
                (\address value -> do
                   (addressMapping, localAddress) <- ppuDecodeAddress address
                   store PPUDataBus addressMapping localAddress value),
                (do
                   softwareState <- getSoftwareState
                   let memory =
                         softwareStateMotherboardPPUTableMemory softwareState
                   return (\offset -> memory ! fromIntegral offset)),
                (do
                   softwareState <- getSoftwareState
                   return $ softwareStatePPUState softwareState),
                (\ppuState -> do
                   softwareState <- getSoftwareState
                   let softwareState' =
                         softwareState {
                             softwareStatePPUState = ppuState
                           }
                   putSoftwareState softwareState'))


cycle :: MonadicState ()
{-# INLINE cycle #-}
cycle = do
  softwareState <- getSoftwareState
  let clockCount = softwareStateMotherboardClockCount softwareState
  mapM_ (\(divisor, chip) -> do
           if mod clockCount divisor == 0
             then case chip of
               CPU_6502 -> CPU.cycle cpuCallbacks
               PPU_NES -> PPU.cycle ppuCallbacks
             else return ())
        [(4, PPU_NES),
         (12, CPU_6502)]
  softwareState <- getSoftwareState
  let clockCount' = mod (clockCount + 1) 12
      !softwareState' = softwareState {
                            softwareStateMotherboardClockCount = clockCount'
                          }
  putSoftwareState softwareState'


getAtCPUCycle :: MonadicState Bool
getAtCPUCycle = do
  softwareState <- getSoftwareState
  let clockCount = softwareStateMotherboardClockCount softwareState
  return $ mod clockCount 12 == 0


getAboutToBeginInstruction :: MonadicState Bool
getAboutToBeginInstruction = do
  softwareState <- getSoftwareState
  atCPUCycle <- getAtCPUCycle
  let cpuState = softwareStateCPUState softwareState
      atInstructionStart =
        CPU.atInstructionStart cpuState
        && atCPUCycle
  return atInstructionStart


disassembleUpcomingInstruction :: MonadicState String
disassembleUpcomingInstruction = do
  state <- getState
  softwareState <- getSoftwareState
  let cpuState = softwareStateCPUState softwareState
      ppuState = softwareStatePPUState softwareState
      debugFetch' address =
        let (result, _) =
              runMonadicState
               (do
                 (addressMapping, localAddress) <- cpuDecodeAddress address
                 debugFetch CPUDataBus addressMapping localAddress)
               state
        in result
      disassembly = CPU.disassembleInstruction
                     cpuState
                     debugFetch'
                     [("CYC",
                       leftPad (show $ PPU.ppuNESStateHorizontalClock ppuState)
                               3),
                      ("SL",
                       show $ PPU.ppuNESStateVerticalClock ppuState),
                      ("F",
                       case PPU.ppuNESStateLatestCompleteFrame ppuState of
                         Nothing -> "no"
                         Just _ -> "yes")]
  return disassembly
