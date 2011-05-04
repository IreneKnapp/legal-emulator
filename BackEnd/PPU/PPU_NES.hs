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

import Debug.Trace
import Assembly


data PPU_NES_State =
  PPU_NES_State {
      ppuNESStateHorizontalClock :: Int,
      ppuNESStateVerticalClock :: Int,
      ppuNESStateStillPoweringUp :: Bool,
      ppuNESStateWantsToAssertNMI :: Bool,
      ppuNESStateAllowedToAssertNMI :: Bool,
      ppuNESStateTallSprites :: Bool,
      ppuNESStatePatternTableForBackground :: Int,
      ppuNESStatePatternTableForSprites :: Int,
      ppuNESStateAddressIncrementVertically :: Bool,
      ppuNESStatePaletteMonochrome :: Bool,
      ppuNESStateBackgroundClipped :: Bool,
      ppuNESStateSpritesClipped :: Bool,
      ppuNESStateBackgroundVisible :: Bool,
      ppuNESStateSpritesVisible :: Bool,
      ppuNESStateIntensifiedColor :: Maybe PrimaryColor,
      ppuNESStateWrittenOddNumberOfTimesToAddresses :: Bool,
      ppuNESStatePermanentAddress :: Word16,
      ppuNESStateTemporaryAddress :: Word16,
      ppuNESStateXOffset :: Word8,
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
  | Address1
  | Address2
  | Access
  deriving (Eq, Show)


data PrimaryColor = Red | Green | Blue
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
      ppuNESStateTallSprites = False,
      ppuNESStatePatternTableForBackground = 0,
      ppuNESStatePatternTableForSprites = 0,
      ppuNESStateAddressIncrementVertically = False,
      ppuNESStatePaletteMonochrome = False,
      ppuNESStateBackgroundClipped = False,
      ppuNESStateSpritesClipped = False,
      ppuNESStateBackgroundVisible = False,
      ppuNESStateSpritesVisible = False,
      ppuNESStateIntensifiedColor = Nothing,
      ppuNESStateWrittenOddNumberOfTimesToAddresses = False,
      ppuNESStatePermanentAddress = 0x0000,
      ppuNESStateTemporaryAddress = 0x0000,
      ppuNESStateXOffset = 0x00,
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
    5 -> Address1
    6 -> Address2
    7 -> Access


registerReadable :: Register -> Bool
registerReadable register =
  case register of
    Control1 -> False
    Control2 -> False
    Status -> True
    SpriteAddress -> False
    SpriteAccess -> False
    Address1 -> False
    Address2 -> False
    Access -> True


registerWriteable :: Register -> Bool
registerWriteable register =
  case register of
    Control1 -> True
    Control2 -> True
    Status -> False
    SpriteAddress -> True
    SpriteAccess -> True
    Address1 -> True
    Address2 -> True
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
            -- TODO other bits
            let wantsToAssertNMI = ppuNESStateWantsToAssertNMI ppuState
                value = foldl (\value (bitIndex, bitValue) ->
                                  if bitValue
                                    then setBit value bitIndex
                                    else clearBit value bitIndex)
                              0x00
                              [(7, wantsToAssertNMI)]
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
registerStore (_, storeByte, _, getState, putState)
              outerState register value =
  let ppuState = getState outerState
      (ppuState', outerState') =
        case register of
          Control1 ->
            let stillPoweringUp = ppuNESStateStillPoweringUp ppuState
                allowedToAssertNMI = testBit value 7
                tallSprites = testBit value 5
                patternTableForBackground =
                  fromIntegral $ shiftR value 4 .&. 0x01
                patternTableForSprites =
                  fromIntegral $ shiftR value 3 .&. 0x01
                addressIncrementVertically = testBit value 2
                nameTable = value .&. 0x03
                permanentAddress = ppuNESStatePermanentAddress ppuState
                permanentAddress' =
                  (permanentAddress .&. 0xF3FF)
                  .|. (shiftL (fromIntegral nameTable) 10)
                ppuState' = ppuState {
                                ppuNESStateAllowedToAssertNMI =
                                  allowedToAssertNMI,
                                ppuNESStateTallSprites = tallSprites,
                                ppuNESStatePatternTableForBackground =
                                  patternTableForBackground,
                                ppuNESStatePatternTableForSprites =
                                  patternTableForSprites,
                                ppuNESStateAddressIncrementVertically =
                                  addressIncrementVertically,
                                ppuNESStatePermanentAddress =
                                  permanentAddress'
                              }
                outerState' = outerState
            in if stillPoweringUp
                 then (ppuState, outerState)
                 else (ppuState', outerState')
          Control2 ->
            let stillPoweringUp = ppuNESStateStillPoweringUp ppuState
                paletteMonochrome = testBit value 0
                backgroundClipped = testBit value 1
                spritesClipped = testBit value 2
                backgroundVisible = testBit value 3
                spritesVisible = testBit value 4
                intensifiedColor =
                  case shiftR value 5 of
                    0x01 -> Just Green
                    0x02 -> Just Blue
                    0x04 -> Just Red
                    _ -> Nothing
                ppuState' = ppuState {
                                ppuNESStatePaletteMonochrome =
                                  paletteMonochrome,
                                ppuNESStateBackgroundClipped =
                                  backgroundClipped,
                                ppuNESStateSpritesClipped =
                                  spritesClipped,
                                ppuNESStateBackgroundVisible =
                                  backgroundVisible,
                                ppuNESStateSpritesVisible =
                                  spritesVisible,
                                ppuNESStateIntensifiedColor =
                                  intensifiedColor
                              }
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
          Address1 ->
            let stillPoweringUp = ppuNESStateStillPoweringUp ppuState
                writtenOddNumberOfTimesToAddresses =
                  ppuNESStateWrittenOddNumberOfTimesToAddresses ppuState
                writtenOddNumberOfTimesToAddresses' =
                  not writtenOddNumberOfTimesToAddresses
                valueHighFive = shiftR value 3 .&. 0x1F
                valueLowThree = value .&. 0x07
                permanentAddress = ppuNESStatePermanentAddress ppuState
                xOffset = ppuNESStateXOffset ppuState
                (permanentAddress', xOffset') =
                  if not writtenOddNumberOfTimesToAddresses
                    then ((permanentAddress .&. 0xFFE0)
                          .|. (fromIntegral valueHighFive),
                          valueLowThree)
                    else ((permanentAddress .&. 0x8C1F)
                          .|. (shiftL (fromIntegral valueHighFive) 5)
                          .|. (shiftL (fromIntegral valueLowThree) 12),
                          xOffset)
                ppuState' = ppuState {
                                ppuNESStateWrittenOddNumberOfTimesToAddresses =
                                  writtenOddNumberOfTimesToAddresses',
                                ppuNESStatePermanentAddress =
                                  permanentAddress',
                                ppuNESStateXOffset = xOffset'
                              }
                outerState' = outerState
            in if stillPoweringUp
                 then (ppuState, outerState)
                 else trace ("Write of $"
                             ++ showHexWord8 value
                             ++ " to Address1.")
                            (ppuState', outerState')
          Address2 ->
            let stillPoweringUp = ppuNESStateStillPoweringUp ppuState
                writtenOddNumberOfTimesToAddresses =
                  ppuNESStateWrittenOddNumberOfTimesToAddresses ppuState
                writtenOddNumberOfTimesToAddresses' =
                  not writtenOddNumberOfTimesToAddresses
                permanentAddress = ppuNESStatePermanentAddress ppuState
                temporaryAddress = ppuNESStateTemporaryAddress ppuState
                (permanentAddress', temporaryAddress') =
                  if not writtenOddNumberOfTimesToAddresses
                    then ((permanentAddress .&. 0x00FF)
                          .|. (shiftL (fromIntegral $ value .&. 0x3F) 8),
                          temporaryAddress)
                    else ((permanentAddress .&. 0xFF00)
                          .|. (fromIntegral value),
                          permanentAddress')
                ppuState' = ppuState {
                                ppuNESStateWrittenOddNumberOfTimesToAddresses =
                                  writtenOddNumberOfTimesToAddresses',
                                ppuNESStatePermanentAddress =
                                  permanentAddress',
                                ppuNESStateTemporaryAddress =
                                  temporaryAddress'
                              }
                outerState' = outerState
            in if stillPoweringUp
                 then (ppuState, outerState)
                 else trace ("Write of $"
                             ++ showHexWord8 value
                             ++ " to Address2.")
                            (ppuState', outerState')
          Access ->
            let addressIncrementVertically =
                  ppuNESStateAddressIncrementVertically ppuState
                permanentAddress = ppuNESStatePermanentAddress ppuState
                permanentAddress' =
                  permanentAddress
                  + if addressIncrementVertically
                      then 32
                      else 1
                ppuState' = ppuState {
                                ppuNESStatePermanentAddress =
                                  permanentAddress'
                              }
                outerState' = storeByte outerState permanentAddress value
            in trace ("Write of $"
                      ++ showHexWord8 value
                      ++ " to $"
                      ++ showHexWord16 permanentAddress
                      ++ ".")
                     (ppuState', outerState')
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
      backgroundVisible = ppuNESStateBackgroundVisible ppuState
      spritesVisible = ppuNESStateSpritesVisible ppuState
      forcedBlank = not backgroundVisible && not spritesVisible
      temporaryAddress = ppuNESStateTemporaryAddress ppuState
      permanentAddress = ppuNESStatePermanentAddress ppuState
      permanentAddress' =
        if forcedBlank
          then permanentAddress
          else case (horizontalClock, verticalClock) of
                 (0, 261) -> temporaryAddress
                 (0, _) -> (permanentAddress .&. 0xFBE0)
                           .|. (temporaryAddress .&. 0x041F)
                 _ -> permanentAddress
      ppuState' = ppuState {
                      ppuNESStateHorizontalClock = horizontalClock',
                      ppuNESStateVerticalClock = verticalClock',
                      ppuNESStateStillPoweringUp = stillPoweringUp',
                      ppuNESStateWantsToAssertNMI = wantsToAssertNMI',
                      ppuNESStatePermanentAddress = permanentAddress'
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
