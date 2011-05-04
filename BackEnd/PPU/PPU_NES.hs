module PPU.PPU_NES
  (
   PPU_NES_State(..),
   Register(..),
   VideoFrame(..),
   powerOnState,
   decodeRegister,
   registerReadable,
   registerWriteable,
   registerFetch,
   registerStore,
   assertingNMI,
   cycle
  )
  where

import Data.Array.Unboxed
import Data.Bits
import Data.Word
import Prelude hiding (cycle)


data PPU_NES_State =
  PPU_NES_State {
      ppuNESStateHorizontalClock :: Int,
      ppuNESStateVerticalClock :: Int,
      ppuNESStateStillPoweringUp :: Bool,
      ppuNESStateWantsToAssertNMI :: Bool,
      ppuNESStateAllowedToAssertNMI :: Bool,
      ppuNESStateWrittenOddNumberOfTimesToAddresses :: Bool,
      ppuNESStateIncompleteFrame :: IncompleteVideoFrame,
      ppuNESStateLatestCompleteFrame :: Maybe VideoFrame
      --ppuNESStateChanges :: [(Int, Int, PPUChange)]
    }


data Register
  = Control1
  | Control2
  | Status
  | SpriteAddress
  | SpriteAccess
  | PermanentAddress
  | TemporaryAddress
  | Access
  deriving (Eq, Show)


data IncompleteVideoFrame =
  IncompleteVideoFrame {
      incompleteVideoFrameNameTableMemory :: Word16 -> Word8
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
      ppuNESStateAllowedToAssertNMI = False,
      ppuNESStateWrittenOddNumberOfTimesToAddresses = False,
      ppuNESStateIncompleteFrame = blankIncompleteVideoFrame,
      ppuNESStateLatestCompleteFrame = Nothing
    }


decodeRegister :: Int -> Register
decodeRegister offset =
  case offset of
    0 -> Control1
    1 -> Control2
    2 -> Status
    3 -> SpriteAddress
    4 -> SpriteAccess
    5 -> PermanentAddress
    6 -> TemporaryAddress
    7 -> Access


registerReadable :: Register -> Bool
registerReadable register =
  case register of
    Control1 -> False
    Control2 -> False
    Status -> True
    SpriteAddress -> False
    SpriteAccess -> False
    PermanentAddress -> False
    TemporaryAddress -> False
    Access -> True


registerWriteable :: Register -> Bool
registerWriteable register =
  case register of
    Control1 -> True
    Control2 -> True
    Status -> False
    SpriteAddress -> True
    SpriteAccess -> True
    PermanentAddress -> True
    TemporaryAddress -> True
    Access -> True


registerFetch :: ((outerState -> Word16 -> (Word8, outerState)),
                  (outerState -> Word16 -> Word8 -> outerState),
                  (outerState -> (Word16 -> Word8)),
                  (outerState -> PPU_NES_State),
                  (outerState -> PPU_NES_State -> outerState))
              -> outerState
              -> Register
              -> (outerState, Word8)
registerFetch (_, _, _, getState, putState) outerState register =
  let ppuState = getState outerState
      (ppuState', value) =
        case register of
          Status ->
            let wantsToAssertNMI = ppuNESStateWantsToAssertNMI ppuState
                value = foldl (\value (bitIndex, bitValue) ->
                                  if bitValue
                                    then setBit value bitIndex
                                    else clearBit value bitIndex)
                              0x00
                              $ [(7, wantsToAssertNMI)]
                ppuState' =
                  ppuState {
                      ppuNESStateWantsToAssertNMI = False,
                      ppuNESStateWrittenOddNumberOfTimesToAddresses = False
                    }
            in (ppuState', value)
          Access ->
            let value = 0x00
                ppuState' = ppuState
            in (ppuState', value)
      outerState' = putState outerState ppuState'
  in (outerState', value)


registerStore :: ((outerState -> Word16 -> (Word8, outerState)),
                  (outerState -> Word16 -> Word8 -> outerState),
                  (outerState -> (Word16 -> Word8)),
                  (outerState -> PPU_NES_State),
                  (outerState -> PPU_NES_State -> outerState))
              -> outerState
              -> Register
              -> Word8
              -> outerState
registerStore (_, _, _, getState, putState) outerState register value =
  let ppuState = getState outerState
      (ppuState', outerState') =
        case register of
          Control1 ->
            let stillPoweringUp = ppuNESStateStillPoweringUp ppuState
                allowedToAssertNMI' = testBit value 7
                ppuState' = ppuState {
                                ppuNESStateAllowedToAssertNMI =
                                  allowedToAssertNMI'
                              }
                outerState' = outerState
            in if stillPoweringUp
                 then (ppuState, outerState)
                 else (ppuState', outerState')
          Control2 ->
            let stillPoweringUp = ppuNESStateStillPoweringUp ppuState
                ppuState' = ppuState
                outerState' = outerState
            in if stillPoweringUp
                 then (ppuState, outerState)
                 else (ppuState', outerState')
          SpriteAddress ->
            let ppuState' = ppuState
                outerState' = outerState
            in (ppuState', outerState')
          SpriteAccess ->
            let ppuState' = ppuState
                outerState' = outerState
            in (ppuState', outerState')
          PermanentAddress ->
            let stillPoweringUp = ppuNESStateStillPoweringUp ppuState
                ppuState' = ppuState
                outerState' = outerState
            in if stillPoweringUp
                 then (ppuState, outerState)
                 else (ppuState', outerState')
          TemporaryAddress ->
            let stillPoweringUp = ppuNESStateStillPoweringUp ppuState
                ppuState' = ppuState
                outerState' = outerState
            in if stillPoweringUp
                 then (ppuState, outerState)
                 else (ppuState', outerState')
          Access ->
            let ppuState' = ppuState
                outerState' = outerState
            in (ppuState', outerState')
      outerState'' = putState outerState' ppuState'
  in outerState''


assertingNMI :: PPU_NES_State -> Bool
assertingNMI ppuState =
  let wantsToAssertNMI = ppuNESStateWantsToAssertNMI ppuState
      allowedToAssertNMI = ppuNESStateAllowedToAssertNMI ppuState
  in wantsToAssertNMI && allowedToAssertNMI


cycle :: ((outerState -> Word16 -> (Word8, outerState)),
          (outerState -> Word16 -> Word8 -> outerState),
          (outerState -> (Word16 -> Word8)),
          (outerState -> PPU_NES_State),
          (outerState -> PPU_NES_State -> outerState))
      -> outerState
      -> outerState
cycle (fetchByte, storeByte, getTableMemory, getState, putState) outerState =
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
          (_, 0, 241) -> True
          (_, 0, 260) -> False
          (oldValue, _, _) -> oldValue
      ppuState' = ppuState {
                      ppuNESStateHorizontalClock = horizontalClock',
                      ppuNESStateVerticalClock = verticalClock',
                      ppuNESStateStillPoweringUp = stillPoweringUp',
                      ppuNESStateWantsToAssertNMI = wantsToAssertNMI'
                    }
      incompleteFrame =
        case (horizontalClock, verticalClock) of
          (0, 240) -> blankIncompleteVideoFrame
          (0, 0) ->
            let incompleteFrame = ppuNESStateIncompleteFrame ppuState'
            in incompleteFrame {
                   incompleteVideoFrameNameTableMemory =
                     getTableMemory outerState
                 }
          _ -> ppuNESStateIncompleteFrame ppuState'
      completeFrame =
        case (horizontalClock, verticalClock) of
          (0, 240) -> Just $ computeVideoFrame ppuState'
          _ -> ppuNESStateLatestCompleteFrame ppuState'
      ppuState'' = ppuState' {
                      ppuNESStateIncompleteFrame = incompleteFrame,
                      ppuNESStateLatestCompleteFrame = completeFrame
                    }
      outerState' = putState outerState ppuState''
  in outerState'


blankIncompleteVideoFrame :: IncompleteVideoFrame
blankIncompleteVideoFrame =
  IncompleteVideoFrame {
      incompleteVideoFrameNameTableMemory = (\_ -> 0x00)
    }


computeVideoFrame :: PPU_NES_State -> VideoFrame
computeVideoFrame ppuState =
  let incompleteFrame = ppuNESStateIncompleteFrame ppuState
      nameTableMemory = incompleteVideoFrameNameTableMemory incompleteFrame
  in VideoFrame {
        videoFrameNameTable =
          array ((0, 0), (32, 29))
                $ map (\index@(x, y) ->
                         let name = nameTableMemory
                                     $ fromIntegral $ y * 32 + x
                         in (index, name))
                      [(x, y) | y <- [0 .. 29], x <- [0 .. 32]]
      }
