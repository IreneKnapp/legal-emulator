{-# LANGUAGE BangPatterns, Rank2Types, TypeSynonymInstances #-}
module Motherboard.NES
  (
   Mirroring(..),
   System(..),
   State(..),
   HardwareState(..),
   SoftwareState(..),
   getState,
   putState,
   runMonadicState,
   powerOnSoftwareState,
   cpuDecodeAddress,
   ppuDecodeAddress,
   debugFetch,
   fetch,
   store,
   cycle,
   atCPUCycle,
   aboutToBeginInstruction,
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


newtype MonadicState a =
  MonadicState (forall r . (a -> State -> r) -> State -> r)


instance Monad MonadicState where
  return value = MonadicState (\continuation state -> continuation value state)
  (MonadicState a) >>= b =
    MonadicState (\continuation state ->
                    a (\intermediate state' ->
                         let MonadicState b' = b intermediate
                         in b' continuation state')
                      state)


runMonadicState :: MonadicState a -> State -> (a, State)
runMonadicState (MonadicState action) state =
  action (\result state' -> (result, state')) state


getState :: MonadicState State
getState = MonadicState
            (\continuation state -> continuation state state)


putState :: State -> MonadicState ()
putState state' = MonadicState
                   (\continuation state -> continuation () state')


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


cpuDecodeAddress :: State
                 -> Word16
                 -> (AddressMapping, Int)
cpuDecodeAddress state address =
  let hardwareState = stateHardwareState state
      programReadOnlyMemory = hardwareStateProgramReadOnlyMemory hardwareState
      programReadOnlyMemoryBankSize = 0x4000
      nProgramReadOnlyMemoryBanks =
        div (1 + (snd $ bounds programReadOnlyMemory))
            programReadOnlyMemoryBankSize
      bankOffset bankIndex = bankIndex * programReadOnlyMemoryBankSize
      lowBankIndex = 0
      highBankIndex = if nProgramReadOnlyMemoryBanks < 2
                        then 0
                        else 1
  in case () of
       () | address < 0x2000 -> (MotherboardCPUMemory,
                                 fromIntegral $ mod address 0x0800)
          | address < 0x4000 -> (PPURegisters,
                                 fromIntegral $ mod address 0x0008)
          | address < 0x8000 -> (NoMemory, 0)
          | address < 0xC000 -> (ProgramReadOnlyMemory,
                                 (fromIntegral $ address - 0x8000)
                                 + bankOffset lowBankIndex)
          | otherwise -> (ProgramReadOnlyMemory,
                          (fromIntegral $ address - 0xC000)
                          + bankOffset highBankIndex)


ppuDecodeAddress :: State
                 -> Word16
                 -> (AddressMapping, Int)
ppuDecodeAddress state address =
  case mod address 0x4000 of
    address'
      | address' < 0x2000 -> (CharacterReadOnlyMemory, fromIntegral address')
      | address' < 0x3F00 ->
          let tableIndex = div (mod (address' - 0x2000) 0x1000) 0x0400
              tableOffset = fromIntegral $ mod (address' - 0x2000) 0x0400
          in case tableIndex of
               0 -> (MotherboardPPUTableMemory,
                     0x0000 + tableOffset)
               1 -> (MotherboardPPUTableMemory,
                     0x0400 + tableOffset)
               2 -> (MotherboardPPUTableMemory,
                     0x0000 + tableOffset)
               3 -> (MotherboardPPUTableMemory,
                     0x0400 + tableOffset)
      | otherwise ->
          (MotherboardPPUPaletteMemory,
           fromIntegral $ mod (address' - 0x3F00) 0x20)


debugFetch :: State
           -> DataBus
           -> AddressMapping
           -> Int
           -> Word8
debugFetch state dataBus addressMapping offset =
  case addressMapping of
    MotherboardCPUMemory ->
      let softwareState = stateSoftwareState state
          memory = softwareStateMotherboardCPUMemory softwareState
      in memory ! offset
    MotherboardPPUTableMemory ->
      let softwareState = stateSoftwareState state
          memory = softwareStateMotherboardPPUTableMemory softwareState
      in memory ! offset
    MotherboardPPUPaletteMemory ->
      let softwareState = stateSoftwareState state
          memory = softwareStateMotherboardPPUPaletteMemory softwareState
      in memory ! offset
    MotherboardPPUSpriteMemory ->
      let softwareState = stateSoftwareState state
          memory = softwareStateMotherboardPPUSpriteMemory softwareState
      in memory ! offset
    ProgramReadOnlyMemory ->
      let hardwareState = stateHardwareState state
          memory = hardwareStateProgramReadOnlyMemory hardwareState
      in memory ! offset
    CharacterReadOnlyMemory ->
      let hardwareState = stateHardwareState state
          memory = hardwareStateCharacterReadOnlyMemory hardwareState
      in memory ! offset
    PPURegisters ->
      0x00
    NoMemory ->
      lastDataBusValue state dataBus


fetch :: DataBus
      -> AddressMapping
      -> Int
      -> MonadicState Word8
fetch dataBus addressMapping offset = do
  state <- getState
  value <- case addressMapping of
             MotherboardCPUMemory -> do
               let !softwareState = stateSoftwareState state
                   !memory = softwareStateMotherboardCPUMemory softwareState
                   !value = memory ! offset
               return value
             MotherboardPPUTableMemory -> do
               let !softwareState = stateSoftwareState state
                   !memory = softwareStateMotherboardPPUTableMemory
                              softwareState
                   !value = memory ! offset
               return value
             MotherboardPPUPaletteMemory -> do
               let !softwareState = stateSoftwareState state
                   !memory = softwareStateMotherboardPPUPaletteMemory
                              softwareState
                   !value = memory ! offset
               return value
             MotherboardPPUSpriteMemory -> do
               let !softwareState = stateSoftwareState state
                   !memory = softwareStateMotherboardPPUSpriteMemory
                              softwareState
                   !value = memory ! offset
               return value
             ProgramReadOnlyMemory -> do
               let !hardwareState = stateHardwareState state
                   !memory = hardwareStateProgramReadOnlyMemory hardwareState
                   !value = memory ! offset
               return value
             CharacterReadOnlyMemory -> do
               let !hardwareState = stateHardwareState state
                   !memory = hardwareStateCharacterReadOnlyMemory hardwareState
                   !value = memory ! offset
               return value
             PPURegisters -> do
               let !register = PPU.decodeRegister offset
                   !readable = PPU.registerReadable register
               if readable
                 then PPU.registerFetch ppuCallbacks register
                 else return $ lastDataBusValue state dataBus
             NoMemory -> do
               let !value = lastDataBusValue state dataBus
               return value
  putLastDataBusValue dataBus value
  return value


store :: DataBus
      -> AddressMapping
      -> Int
      -> Word8
      -> MonadicState ()
store dataBus addressMapping offset value = do
  state <- getState
  case addressMapping of
    MotherboardCPUMemory -> do
      let !softwareState = stateSoftwareState state
          !memory = softwareStateMotherboardCPUMemory softwareState
          !memory' = memory // [(offset, value)]
          !softwareState' =
             softwareState {
                 softwareStateMotherboardCPUMemory = memory'
               }
          !state' = state {
                        stateSoftwareState = softwareState'
                      }
      putState state'
    MotherboardPPUTableMemory -> do
      let !softwareState = stateSoftwareState state
          !memory = softwareStateMotherboardPPUTableMemory softwareState
          !memory' = memory // [(offset, value)]
          !softwareState' =
             softwareState {
                 softwareStateMotherboardPPUTableMemory = memory'
               }
          !state' = state {
                        stateSoftwareState = softwareState'
                      }
      putState state'
    MotherboardPPUPaletteMemory -> do
      let !softwareState = stateSoftwareState state
          !memory = softwareStateMotherboardPPUPaletteMemory softwareState
          !memory' = memory // [(offset, value)]
          !softwareState' =
             softwareState {
                 softwareStateMotherboardPPUPaletteMemory = memory'
               }
          !state' = state {
                        stateSoftwareState = softwareState'
                      }
      putState state'
    MotherboardPPUSpriteMemory -> do
      let !softwareState = stateSoftwareState state
          !memory = softwareStateMotherboardPPUSpriteMemory softwareState
          !memory' = memory // [(offset, value)]
          !softwareState' =
             softwareState {
                 softwareStateMotherboardPPUSpriteMemory = memory'
               }
          !state' = state {
                        stateSoftwareState = softwareState'
                      }
      putState state'
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


lastDataBusValue :: State -> DataBus -> Word8
lastDataBusValue state dataBus=
  let softwareState = stateSoftwareState state
  in case dataBus of
       CPUDataBus -> softwareStateLastCPUDataBusValue softwareState
       PPUDataBus -> softwareStateLastPPUDataBusValue softwareState


putLastDataBusValue :: DataBus -> Word8 -> MonadicState ()
putLastDataBusValue dataBus value = do
  state <- getState
  let softwareState = stateSoftwareState state
      softwareState' =
        case dataBus of
          CPUDataBus ->
            softwareState {
                softwareStateLastCPUDataBusValue = value
              }
          PPUDataBus ->
            softwareState {
                softwareStateLastPPUDataBusValue = value
              }
      state' = state {
                   stateSoftwareState = softwareState'
                 }
  putState state'


cpuCallbacks :: ((Word16 -> MonadicState Word8),
                 (Word16 -> Word8 -> MonadicState ()),
                 (MonadicState Bool),
                 (MonadicState Bool),
                 (MonadicState CPU.CPU_6502_State),
                 (CPU.CPU_6502_State -> MonadicState ()))
cpuCallbacks = ((\address -> do
                   state <- getState
                   let (!addressMapping, !localAddress) =
                         cpuDecodeAddress state address
                   fetch CPUDataBus addressMapping localAddress),
                (\address value -> do
                   state <- getState
                   let (!addressMapping, !localAddress) =
                         cpuDecodeAddress state address
                   store CPUDataBus addressMapping localAddress value),
                (return False),
                (do
                   state <- getState
                   let softwareState = stateSoftwareState state
                       ppuState = softwareStatePPUState softwareState
                       nmiAsserted = PPU.assertingNMI ppuState
                   return nmiAsserted),
                (do
                   state <- getState
                   return $ softwareStateCPUState $ stateSoftwareState state),
                (\cpuState -> do
                   state <- getState
                   let softwareState = stateSoftwareState state
                       softwareState' =
                         softwareState {
                             softwareStateCPUState = cpuState
                           }
                       state' = state {
                                    stateSoftwareState = softwareState'
                                  }
                   putState state'))


ppuCallbacks :: ((Word16 -> MonadicState Word8),
                 (Word16 -> Word8 -> MonadicState ()),
                 (MonadicState (Word16 -> Word8)),
                 (MonadicState PPU.PPU_NES_State),
                 (PPU.PPU_NES_State -> MonadicState ()))
ppuCallbacks = ((\address -> do
                   state <- getState
                   let (!addressMapping, !localAddress) =
                         ppuDecodeAddress state address
                   fetch PPUDataBus addressMapping localAddress),
                (\address value -> do
                   state <- getState
                   let (!addressMapping, !localAddress) =
                         ppuDecodeAddress state address
                   store PPUDataBus addressMapping localAddress value),
                (do
                   state <- getState
                   let softwareState = stateSoftwareState state
                       memory =
                         softwareStateMotherboardPPUTableMemory softwareState
                   return (\offset -> memory ! fromIntegral offset)),
                (do
                   state <- getState
                   return $ softwareStatePPUState $ stateSoftwareState state),
                (\ppuState -> do
                   state <- getState
                   let softwareState = stateSoftwareState state
                       softwareState' =
                         softwareState {
                             softwareStatePPUState = ppuState
                           }
                       state' = state {
                                    stateSoftwareState = softwareState'
                                  }
                   putState state'))


cycle :: MonadicState ()
{-# INLINE cycle #-}
cycle = do
  state <- getState
  let clockCount =
        softwareStateMotherboardClockCount $ stateSoftwareState state
  mapM_ (\(divisor, chip) -> do
           if mod clockCount divisor == 0
             then case chip of
               CPU_6502 -> CPU.cycle cpuCallbacks
               PPU_NES -> PPU.cycle ppuCallbacks
             else return ())
        [(4, PPU_NES),
         (12, CPU_6502)]
  state <- getState
  let clockCount' = mod (clockCount + 1) 12
      softwareState = stateSoftwareState state
      !softwareState' = softwareState {
                            softwareStateMotherboardClockCount = clockCount'
                          }
      !state' = state {
                    stateSoftwareState = softwareState'
                  }
  putState state'


atCPUCycle :: State -> Bool
atCPUCycle state =
  let softwareState = stateSoftwareState state
      clockCount = softwareStateMotherboardClockCount softwareState
  in mod clockCount 12 == 0


aboutToBeginInstruction :: State -> Bool
aboutToBeginInstruction state =
  let softwareState = stateSoftwareState state
      cpuState = softwareStateCPUState softwareState
      atInstructionStart =
        CPU.atInstructionStart cpuState
        && atCPUCycle state
  in atInstructionStart


disassembleUpcomingInstruction :: State -> String
disassembleUpcomingInstruction state =
  let softwareState = stateSoftwareState state
      cpuState = softwareStateCPUState softwareState
      ppuState = softwareStatePPUState softwareState
      debugFetch' address =
        let (addressMapping, localAddress) =
              cpuDecodeAddress state address
        in debugFetch state CPUDataBus addressMapping localAddress
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
  in disassembly
