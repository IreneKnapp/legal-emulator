module Motherboard.NES
  (
   Mirroring(..),
   System(..),
   HardwareState(..),
   
  )
  where

import Data.Array.Unboxed
import Data.Word


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
      hardwareStatePlayChoice10HintScreen :: Maybe (UArray Int Word8)
    }
