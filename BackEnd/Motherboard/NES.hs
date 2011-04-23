module Motherboard.NES
  (
   Mirroring(..),
   System(..),
   HardwareState(..),
   SoftwareState(..),
   AddressMapping(..),
   nesPowerOnSoftwareState,
   cpuDecodeAddress,
   cpuFetch,
   cpuStore,
   cpuCycle
  )
  where

import Data.Array.Unboxed
import Data.Word

import Processor.CPU6502


data Mirroring = HorizontalMirroring
               | VerticalMirroring
               | FourScreenMirroring


data System = PlainSystem
            | VersusUnisystem
            | PlayChoice10
            deriving (Eq, Show)


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
      softwareStateCPUState :: CPU6502State,
      softwareStateMotherboardMemory :: UArray Int Word8
    }


data AddressMapping = MotherboardMemory
                    | ProgramReadOnlyMemoryBank Int
                    | CharacterReadOnlyMemoryBank Int
                    | NoMemory


nesPowerOnSoftwareState :: SoftwareState
nesPowerOnSoftwareState =
  SoftwareState {
      softwareStateCPUState = cpu6502PowerOnState,
      softwareStateMotherboardMemory = array (0x0000, 0x07FF)
                                             $ zip [0x0000 .. 0x07FF]
                                                   $ repeat 0
    }


cpuDecodeAddress :: HardwareState
                 -> SoftwareState
                 -> Word16
                 -> (AddressMapping, Word16)
cpuDecodeAddress hardwareState softwareState address =
  case () of
    () | address < 0x0800 -> (MotherboardMemory, address)
       | address < 0x8000 -> (NoMemory, 0)
       | address < 0xC000 -> (ProgramReadOnlyMemoryBank 0, address - 0x8000)
       | otherwise -> (ProgramReadOnlyMemoryBank 0, address - 0xC000)


cpuFetch :: HardwareState
         -> SoftwareState
         -> Word16
         -> (SoftwareState, Word8)
cpuFetch hardwareState softwareState address =
  case cpuDecodeAddress hardwareState softwareState address of
    (MotherboardMemory, offset) ->
      (softwareState,
       softwareStateMotherboardMemory softwareState ! fromIntegral offset)
    (ProgramReadOnlyMemoryBank bank, offset) ->
      (softwareState,
       hardwareStateProgramReadOnlyMemory hardwareState
        ! ((bank * 16384) + fromIntegral offset))
    (NoMemory, _) -> (softwareState, 0)


cpuStore :: HardwareState
         -> SoftwareState
         -> Word16
         -> Word8
         -> SoftwareState
cpuStore hardwareState softwareState address value =
  case cpuDecodeAddress hardwareState softwareState address of
    (MotherboardMemory, offset) ->
      let workingMemory' =
            softwareStateMotherboardMemory softwareState
             // [(fromIntegral offset, value)]
      in softwareState {
             softwareStateMotherboardMemory = workingMemory'
           }
    (ProgramReadOnlyMemoryBank _, _) -> softwareState
    (NoMemory, _) -> softwareState


cpuCycle :: HardwareState
         -> SoftwareState
         -> SoftwareState
cpuCycle hardwareState softwareState =
  let (_, softwareState') =
        cpu6502Cycle ((\(hardwareState, softwareState) address ->
                          let (softwareState', byte) =
                                cpuFetch hardwareState
                                         softwareState
                                         address
                          in (byte, (hardwareState, softwareState'))),
                      (\(hardwareState, softwareState) address byte ->
                          let softwareState' =
                                cpuStore hardwareState
                                         softwareState
                                         address
                                         byte
                          in (hardwareState, softwareState')),
                      (\(_, softwareState) ->
                          softwareStateCPUState softwareState),
                      (\(hardwareState, softwareState) cpuState ->
                          (hardwareState,
                           softwareState {
                               softwareStateCPUState = cpuState
                             })))
                     (hardwareState, softwareState)
  in softwareState'
