module Motherboard.NES
  (
   Mirroring(..),
   System(..),
   State(..),
   HardwareState(..),
   SoftwareState(..),
   AddressMapping(..),
   Processor(..),
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

import Data.Array.Unboxed
import qualified Data.ByteString as BS
import Data.Word
import Prelude hiding (cycle)

import Assembly
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
      stateHardwareState :: HardwareState,
      stateSoftwareState :: SoftwareState
      -- stateConsoleOutputBuffer :: ByteString
    }


data HardwareState =
  HardwareState {
      hardwareStateProgramReadOnlyMemory :: UArray Int Word8,
      hardwareStateCharacterReadOnlyMemory :: UArray Int Word8,
      hardwareStateTrainer :: Maybe (UArray Int Word8),
      hardwareStatePlayChoice10HintScreen :: Maybe (UArray Int Word8),
      hardwareStateMapperNumber :: Word8,
      hardwareStateMirroringType :: Mirroring,
      hardwareStateBatteryPresent :: Bool,
      hardwareStateSystem :: System
    }


data SoftwareState =
  SoftwareState {
      softwareStateMotherboardClockCount :: Int,
      softwareStateLastCPUDataBusValue :: Word8,
      softwareStateLastPPUDataBusValue :: Word8,
      softwareStateCPUState :: CPU.CPU_6502_State,
      softwareStatePPUState :: PPU.PPU_NES_State,
      softwareStateMotherboardCPUMemory :: UArray Int Word8,
      softwareStateMotherboardPPUTableMemory :: UArray Int Word8,
      softwareStateMotherboardPPUPaletteMemory :: UArray Int Word8,
      softwareStateMotherboardPPUSpriteMemory :: UArray Int Word8
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


fetch :: State
      -> DataBus
      -> AddressMapping
      -> Int
      -> (State, Word8)
fetch state dataBus addressMapping offset =
  let (state', value) =
        case addressMapping of
          MotherboardCPUMemory ->
            let softwareState = stateSoftwareState state
                memory = softwareStateMotherboardCPUMemory softwareState
                value = memory ! offset
            in (state, value)
          MotherboardPPUTableMemory ->
            let softwareState = stateSoftwareState state
                memory = softwareStateMotherboardPPUTableMemory softwareState
                value = memory ! offset
            in (state, value)
          MotherboardPPUPaletteMemory ->
            let softwareState = stateSoftwareState state
                memory = softwareStateMotherboardPPUPaletteMemory softwareState
                value = memory ! offset
            in (state, value)
          MotherboardPPUSpriteMemory ->
            let softwareState = stateSoftwareState state
                memory = softwareStateMotherboardPPUSpriteMemory softwareState
                value = memory ! offset
            in (state, value)
          ProgramReadOnlyMemory ->
            let hardwareState = stateHardwareState state
                memory = hardwareStateProgramReadOnlyMemory hardwareState
                value = memory ! offset
            in (state, value)
          CharacterReadOnlyMemory ->
            let hardwareState = stateHardwareState state
                memory = hardwareStateCharacterReadOnlyMemory hardwareState
                value = memory ! offset
            in (state, value)
          PPURegisters ->
            let register = PPU.decodeRegister offset
                readable = PPU.registerReadable register
            in if readable
                 then PPU.registerFetch ppuCallbacks state register
                 else let value = lastDataBusValue state dataBus
                      in (state, value)
          NoMemory ->
            let value = lastDataBusValue state dataBus
            in (state, value)
  in (updateLastDataBusValue state' dataBus value, value)


store :: State
      -> DataBus
      -> AddressMapping
      -> Int
      -> Word8
      -> State
store state dataBus addressMapping offset value =
  let state' =
        case addressMapping of
          MotherboardCPUMemory ->
             let softwareState = stateSoftwareState state
                 memory =
                   softwareStateMotherboardCPUMemory softwareState
                 memory' =
                   memory // [(offset, value)]
                 softwareState' =
                   softwareState {
                       softwareStateMotherboardCPUMemory = memory'
                     }
             in state {
                    stateSoftwareState = softwareState'
                  }
          MotherboardPPUTableMemory ->
             let softwareState = stateSoftwareState state
                 memory =
                   softwareStateMotherboardPPUTableMemory softwareState
                 memory' =
                   memory // [(offset, value)]
                 softwareState' =
                   softwareState {
                       softwareStateMotherboardPPUTableMemory = memory'
                     }
             in state {
                    stateSoftwareState = softwareState'
                  }
          MotherboardPPUPaletteMemory ->
             let softwareState = stateSoftwareState state
                 memory =
                   softwareStateMotherboardPPUPaletteMemory softwareState
                 memory' =
                   memory // [(offset, value)]
                 softwareState' =
                   softwareState {
                       softwareStateMotherboardPPUPaletteMemory = memory'
                     }
             in state {
                    stateSoftwareState = softwareState'
                  }
          MotherboardPPUSpriteMemory ->
             let softwareState = stateSoftwareState state
                 memory =
                   softwareStateMotherboardPPUSpriteMemory softwareState
                 memory' =
                   memory // [(offset, value)]
                 softwareState' =
                   softwareState {
                       softwareStateMotherboardPPUSpriteMemory = memory'
                     }
             in state {
                    stateSoftwareState = softwareState'
                  }
          ProgramReadOnlyMemory -> state
          CharacterReadOnlyMemory -> state
          PPURegisters ->
            let register = PPU.decodeRegister offset
                writeable = PPU.registerWriteable register
            in if writeable
                 then PPU.registerStore ppuCallbacks state register value
                 else state
          NoMemory -> state
  in updateLastDataBusValue state' dataBus value


lastDataBusValue :: State -> DataBus -> Word8
lastDataBusValue state dataBus=
  let softwareState = stateSoftwareState state
  in case dataBus of
       CPUDataBus -> softwareStateLastCPUDataBusValue softwareState
       PPUDataBus -> softwareStateLastPPUDataBusValue softwareState


updateLastDataBusValue :: State -> DataBus -> Word8 -> State
updateLastDataBusValue state dataBus value =
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
  in state {
         stateSoftwareState = softwareState'
       }


cpuCallbacks :: ((State -> Word16 -> (Word8, State)),
                 (State -> Word16 -> Word8 -> State),
                 (State -> Bool),
                 (State -> Bool),
                 (State -> CPU.CPU_6502_State),
                 (State -> CPU.CPU_6502_State -> State))
cpuCallbacks = ((\state address ->
                   let (addressMapping, localAddress) =
                         cpuDecodeAddress state address
                       (state', value) =
                         fetch state
                               CPUDataBus
                               addressMapping
                               localAddress
                   in (value, state')),
                (\state address value ->
                   let (addressMapping, localAddress) =
                         cpuDecodeAddress state address
                   in store state
                            CPUDataBus
                            addressMapping
                            localAddress
                            value),
                (\_ -> False),
                (\state ->
                  let softwareState = stateSoftwareState state
                      ppuState = softwareStatePPUState softwareState
                      nmiAsserted = PPU.assertingNMI ppuState
                  in nmiAsserted),
                (\state -> softwareStateCPUState $ stateSoftwareState state),
                (\state cpuState ->
                   let softwareState = stateSoftwareState state
                       softwareState' =
                         softwareState {
                             softwareStateCPUState = cpuState
                           }
                   in state {
                          stateSoftwareState = softwareState'
                        }))


ppuCallbacks :: ((State -> Word16 -> (Word8, State)),
                 (State -> Word16 -> Word8 -> State),
                 (State -> (Word16 -> Word8)),
                 (State -> PPU.PPU_NES_State),
                 (State -> PPU.PPU_NES_State -> State))
ppuCallbacks = ((\state address ->
                   let (addressMapping, localAddress) =
                         ppuDecodeAddress state address
                       (state', value) =
                         fetch state PPUDataBus addressMapping localAddress
                   in (value, state')),
                (\state address value ->
                   let (addressMapping, localAddress) =
                         ppuDecodeAddress state address
                   in store state
                            PPUDataBus
                            addressMapping
                            localAddress
                            value),
                (\state ->
                   let softwareState = stateSoftwareState state
                       memory =
                         softwareStateMotherboardPPUTableMemory softwareState
                   in (\offset -> memory ! fromIntegral offset)),
                (\state -> softwareStatePPUState $ stateSoftwareState state),
                (\state ppuState ->
                   let softwareState = stateSoftwareState state
                       softwareState' =
                         softwareState {
                             softwareStatePPUState = ppuState
                           }
                   in state {
                          stateSoftwareState = softwareState'
                        }))


cycle :: State -> State
cycle state =
  let clockCount =
        softwareStateMotherboardClockCount $ stateSoftwareState state
      chipsToCycle = concat $ map (\(divisor, chip) ->
                                      if mod clockCount divisor == 0
                                        then [chip]
                                        else [])
                                  [(4, PPU_NES),
                                   (12, CPU_6502)]
      state' = foldl (\state' chip ->
                        case chip of
                          CPU_6502 -> CPU.cycle cpuCallbacks state'
                          PPU_NES -> PPU.cycle ppuCallbacks state')
                     state
                     chipsToCycle
      clockCount' = mod (clockCount + 1) 12
      softwareState' = stateSoftwareState state'
      softwareState'' = softwareState' {
                            softwareStateMotherboardClockCount = clockCount'
                          }
  in state {
         stateSoftwareState = softwareState''
       }


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
