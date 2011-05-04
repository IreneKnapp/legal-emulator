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
   atCPUCycle
  )
  where

import Data.Array.Unboxed
import Data.Word
import Prelude hiding (cycle)

import qualified Processor.CPU_6502 as CPU
import qualified PPU.PPU_NES as PPU

import Debug.Trace
import Assembly


data Mirroring = HorizontalMirroring
               | VerticalMirroring
               | FourScreenMirroring


data System = PlainSystem
            | VersusUnisystem
            | PlayChoice10
            deriving (Eq, Show)


data State =
  State {
      stateHardwareState :: HardwareState,
      stateSoftwareState :: SoftwareState
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
      softwareStateMotherboardPPUTableMemory
        :: UArray Int Word8,
      softwareStateMotherboardPPUPaletteMemory :: UArray Int Word8,
      softwareStateMotherboardPPUSpriteMemory :: UArray Int Word8
    }


data DataBus = CPUDataBus
             | PPUDataBus


data AddressMapping = MotherboardCPUMemory
                    | MotherboardPPUTableMemory
                    | MotherboardPPUPaletteMemory
                    | MotherboardPPUSpriteMemory
                    | ProgramReadOnlyMemory
                    | CharacterReadOnlyMemory
                    | PPURegisters
                    | NoMemory


data Processor = CPU_6502
               | PPU_NES


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
                 -> (AddressMapping, Word16)
cpuDecodeAddress state address =
  case () of
    () | address < 0x2000 -> (MotherboardCPUMemory, mod address 0x0800)
       | address < 0x4000 -> (PPURegisters, mod address 0x0008)
       | address < 0x8000 -> (NoMemory, 0)
       | address < 0xC000 -> (ProgramReadOnlyMemory, address - 0x8000)
       | otherwise -> (ProgramReadOnlyMemory, address - 0xC000)


ppuDecodeAddress :: State
                 -> Word16
                 -> (AddressMapping, Word16)
ppuDecodeAddress state address =
  case mod address 0x4000 of
    address'
      | address' < 0x2000 -> (CharacterReadOnlyMemory, address')
      | address' < 0x3F00 ->
          let tableIndex = div (mod (address' - 0x2000) 0x1000) 0x0400
              tableOffset = mod (address' - 0x2000) 0x0400
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
          (MotherboardPPUPaletteMemory, mod (address' - 0x3F00) 0x20)


debugFetch :: State
           -> AddressMapping
           -> Word16
           -> Word8
debugFetch state addressMapping offset =
  case addressMapping of
    MotherboardCPUMemory ->
      let softwareState = stateSoftwareState state
          memory = softwareStateMotherboardCPUMemory softwareState
      in memory ! fromIntegral offset
    MotherboardPPUTableMemory ->
      let softwareState = stateSoftwareState state
          memory = softwareStateMotherboardPPUTableMemory softwareState
      in memory ! fromIntegral offset
    MotherboardPPUPaletteMemory ->
      let softwareState = stateSoftwareState state
          memory = softwareStateMotherboardPPUPaletteMemory softwareState
      in memory ! fromIntegral offset
    MotherboardPPUSpriteMemory ->
      let softwareState = stateSoftwareState state
          memory = softwareStateMotherboardPPUSpriteMemory softwareState
      in memory ! fromIntegral offset
    ProgramReadOnlyMemory ->
      let hardwareState = stateHardwareState state
          memory = hardwareStateProgramReadOnlyMemory hardwareState
      in memory ! fromIntegral offset
    CharacterReadOnlyMemory ->
      let hardwareState = stateHardwareState state
          memory = hardwareStateCharacterReadOnlyMemory hardwareState
      in memory ! fromIntegral offset
    PPURegisters ->
      0x00
    NoMemory ->
      0x00


fetch :: State
      -> DataBus
      -> AddressMapping
      -> Word16
      -> (State, Word8)
fetch state dataBus addressMapping offset =
  let (state', value) =
        case addressMapping of
          MotherboardCPUMemory ->
            let softwareState = stateSoftwareState state
                memory = softwareStateMotherboardCPUMemory softwareState
                value = memory ! fromIntegral offset
            in (state, value)
          MotherboardPPUTableMemory ->
            let softwareState = stateSoftwareState state
                memory = softwareStateMotherboardPPUTableMemory softwareState
                value = memory ! fromIntegral offset
            in (state, value)
          MotherboardPPUPaletteMemory ->
            let softwareState = stateSoftwareState state
                memory = softwareStateMotherboardPPUPaletteMemory softwareState
                value = memory ! fromIntegral offset
            in (state, value)
          MotherboardPPUSpriteMemory ->
            let softwareState = stateSoftwareState state
                memory = softwareStateMotherboardPPUSpriteMemory softwareState
                value = memory ! fromIntegral offset
            in (state, value)
          ProgramReadOnlyMemory ->
            let hardwareState = stateHardwareState state
                memory = hardwareStateProgramReadOnlyMemory hardwareState
                value = memory ! fromIntegral offset
            in (state, value)
          PPURegisters ->
            trace ("Read from $"
                   ++ (showHexWord16 $ 0x2000 + offset)
                   ++ ".")
                  (state, 0x00)
          NoMemory ->
            let softwareState = stateSoftwareState state
                value = case dataBus of
                          CPUDataBus ->
                            softwareStateLastCPUDataBusValue softwareState
                          PPUDataBus ->
                            softwareStateLastPPUDataBusValue softwareState
            in (state, value)
  in (updateLastDataBusValue state' dataBus value, value)


store :: State
      -> DataBus
      -> AddressMapping
      -> Word16
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
                   memory // [(fromIntegral offset, value)]
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
                   memory // [(fromIntegral offset, value)]
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
                   memory // [(fromIntegral offset, value)]
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
                   memory // [(fromIntegral offset, value)]
                 softwareState' =
                   softwareState {
                       softwareStateMotherboardPPUSpriteMemory = memory'
                     }
             in state {
                    stateSoftwareState = softwareState'
                  }
          ProgramReadOnlyMemory -> state
          PPURegisters -> trace ("Write $"
                                 ++ (showHexWord8 value)
                                 ++ " to $"
                                 ++ (showHexWord16 $ 0x2000 + offset)
                                 ++ ".")
                                state
          NoMemory -> state
  in updateLastDataBusValue state' CPUDataBus value


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
                 (State -> PPU.PPU_NES_State),
                 (State -> PPU.PPU_NES_State -> State))
ppuCallbacks = ((\state address ->
                   let (addressMapping, localAddress) =
                         ppuDecodeAddress state address
                       (state', value) =
                         fetch state PPUDataBus addressMapping localAddress
                   in (value, state')),
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
