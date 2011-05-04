module PPU.PPU_NES
  (
   PPU_NES_State(..),
   VideoFrame(..),
   powerOnState,
   cycle
  )
  where

import Data.Array.Unboxed
import Data.Word
import Prelude hiding (cycle)


data PPU_NES_State =
  PPU_NES_State {
      ppuNESStateHorizontalClock :: Int,
      ppuNESStateVerticalClock :: Int,
      ppuNESStateStillPoweringUp :: Bool,
      ppuNESStateWantsToAssertNMI :: Bool,
      ppuNESStateSoftwareWantsNMI :: Bool,
      ppuNESStateLatestCompleteFrame :: Maybe VideoFrame
      --ppuNESStateChanges :: [(Int, Int, PPUChagne)]
    }


data VideoFrame =
  VideoFrame {
      videoFrameNameTable :: UArray (Int, Int) Word8
    }


powerOnState :: PPU_NES_State
powerOnState =
  PPU_NES_State {
      ppuNESStateHorizontalClock = 0,
      ppuNESStateVerticalClock = 241,
      ppuNESStateStillPoweringUp = True,
      ppuNESStateWantsToAssertNMI = True,
      ppuNESStateSoftwareWantsNMI = False,
      ppuNESStateLatestCompleteFrame = Nothing
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
      newlyCompleteFrame =
        case (horizontalClock', verticalClock') of
          (0, 240) -> Just $ computeVideoFrame ppuState'
          _ -> ppuNESStateLatestCompleteFrame ppuState'
      ppuState'' = ppuState' {
                      ppuNESStateLatestCompleteFrame = newlyCompleteFrame
                    }
      outerState' = putState outerState ppuState''
  in outerState'


computeVideoFrame :: PPU_NES_State -> VideoFrame
computeVideoFrame ppuState =
  VideoFrame {
      videoFrameNameTable =
        array ((0, 0), (32, 29))
              $ map (\index@(x, y) ->
                       let name = 0x00
                       in (index, name))
                    [(x, y) | y <- [0 .. 29], x <- [0 .. 32]]
    }
