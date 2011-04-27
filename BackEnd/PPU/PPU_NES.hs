module PPU.PPU_NES
  (
   PPU_NES_State(..),
   ppuNESPowerOnState,
   ppuNESCycle
  )
  where

data PPU_NES_State =
  PPU_NES_State {
      ppuNESStateHorizontalClock :: Int,
      ppuNESStateVerticalClock :: Int
    }


ppuNESPowerOnState :: PPU_NES_State
ppuNESPowerOnState =
  PPU_NES_State {
      ppuNESStateHorizontalClock = 0,
      ppuNESStateVerticalClock = 241
    }


ppuNESCycle :: ((outerState -> PPU_NES_State),
                (outerState -> PPU_NES_State -> outerState))
            -> outerState
            -> outerState
ppuNESCycle (getState, putState) outerState =
  let ppuState = getState outerState
      horizontalClock = ppuNESStateHorizontalClock ppuState
      verticalClock = ppuNESStateVerticalClock ppuState
      horizontalClock' = mod (horizontalClock + 1) 341
      verticalClock' = if horizontalClock == 340
                         then if verticalClock == 260
                                then -1
                                else verticalClock + 1
                         else verticalClock
      ppuState' = ppuState {
                      ppuNESStateHorizontalClock = horizontalClock',
                      ppuNESStateVerticalClock = verticalClock'
                    }
      outerState' = putState outerState ppuState'
  in outerState'
