module PPU.PPU_NES
  (
   PPU_NES_State(..),
   powerOnState,
   cycle
  )
  where

import Data.Word
import Prelude hiding (cycle)


data PPU_NES_State =
  PPU_NES_State {
      ppuNESStateHorizontalClock :: Int,
      ppuNESStateVerticalClock :: Int,
      ppuNESStateStillPoweringUp :: Bool,
      ppuNESStateWantsToAssertNMI :: Bool,
      ppuNESStateSoftwareWantsNMI :: Bool
      {-
      ppuNESStateCompleteFrame :: PPUFrame
      ppuNESStateChanges :: [(Int, Int, PPUChagne)]
      -}
    }


powerOnState :: PPU_NES_State
powerOnState =
  PPU_NES_State {
      ppuNESStateHorizontalClock = 0,
      ppuNESStateVerticalClock = 241,
      ppuNESStateStillPoweringUp = True,
      ppuNESStateWantsToAssertNMI = True,
      ppuNESStateSoftwareWantsNMI = False
    }


cycle :: ((outerState -> Word16 -> (Word8, outerState)),
          (outerState -> PPU_NES_State),
          (outerState -> PPU_NES_State -> outerState))
      -> outerState
      -> outerState
cycle (fetchByte, getState, putState) outerState =
  let ppuState = getState outerState
      horizontalClock = ppuNESStateHorizontalClock ppuState
      verticalClock = ppuNESStateVerticalClock ppuState
      horizontalClock' = mod (horizontalClock + 1) 341
      verticalClock' = if horizontalClock == 340
                         then mod (verticalClock + 1) 262
                         else verticalClock
      stillPoweringUp = ppuNESStateStillPoweringUp ppuState
      stillPoweringUp' =
        case (stillPoweringUp, horizontalClock, verticalClock) of
          (False, _, _) -> False
          (True, 314, 260) -> False
          (True, _, _) -> True
      wantsToAssertNMI = ppuNESStateWantsToAssertNMI ppuState
      wantsToAssertNMI' =
        case (wantsToAssertNMI, horizontalClock, verticalClock) of
          (_, 0, 240) -> True
          (_, 0, 260) -> False
          (oldValue, _, _) -> oldValue
      ppuState' = ppuState {
                      ppuNESStateHorizontalClock = horizontalClock',
                      ppuNESStateVerticalClock = verticalClock',
                      ppuNESStateStillPoweringUp = stillPoweringUp',
                      ppuNESStateWantsToAssertNMI = wantsToAssertNMI'
                    }
      outerState' = putState outerState ppuState'
  in outerState'
