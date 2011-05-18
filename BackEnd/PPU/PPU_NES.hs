{-# LANGUAGE BangPatterns #-}
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

import Control.DeepSeq
import Data.Array.Unboxed
import Data.Bits
import Data.Word
import Prelude hiding (cycle, Maybe(..))

import Data.Instances
import Data.Strict.Maybe

import Debug.Trace
import Assembly


data PPU_NES_State =
  PPU_NES_State {
      ppuNESStateHorizontalClock :: ! Int,
      ppuNESStateVerticalClock :: ! Int,
      ppuNESStateStillPoweringUp :: ! Bool,
      ppuNESStateWantsToAssertNMI :: ! Bool,
      ppuNESStateAllowedToAssertNMI :: ! Bool,
      ppuNESStateTallSprites :: ! Bool,
      ppuNESStatePatternTableForBackground :: ! Int,
      ppuNESStatePatternTableForSprites :: ! Int,
      ppuNESStateAddressIncrementVertically :: ! Bool,
      ppuNESStatePaletteMonochrome :: ! Bool,
      ppuNESStateBackgroundClipped :: ! Bool,
      ppuNESStateSpritesClipped :: ! Bool,
      ppuNESStateBackgroundVisible :: ! Bool,
      ppuNESStateSpritesVisible :: ! Bool,
      ppuNESStateIntensifiedColor :: ! (Maybe PrimaryColor),
      ppuNESStateWrittenOddNumberOfTimesToAddresses :: ! Bool,
      ppuNESStatePermanentAddress :: ! Word16,
      ppuNESStateTemporaryAddress :: ! Word16,
      ppuNESStateXOffset :: ! Word8,
      ppuNESStateIncompleteFrame :: ! IncompleteVideoFrame,
      ppuNESStateLatestCompleteFrame :: ! (Maybe VideoFrame)
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
      incompleteVideoFrameNameTableMemory :: ! (Word16 -> Word8)
    }


data VideoFrame =
  VideoFrame {
      videoFrameNameTable :: ! (UArray (Int, Int) Word8)
    }


instance NFData PPU_NES_State where
  rnf ppuState =
    (rnf $ ppuNESStateHorizontalClock ppuState)
    `seq` (rnf $ ppuNESStateVerticalClock ppuState)
    `seq` (rnf $ ppuNESStateStillPoweringUp ppuState)
    `seq` (rnf $ ppuNESStateWantsToAssertNMI ppuState)
    `seq` (rnf $ ppuNESStateAllowedToAssertNMI ppuState)
    `seq` (rnf $ ppuNESStateTallSprites ppuState)
    `seq` (rnf $ ppuNESStatePatternTableForBackground ppuState)
    `seq` (rnf $ ppuNESStatePatternTableForSprites ppuState)
    `seq` (rnf $ ppuNESStateAddressIncrementVertically ppuState)
    `seq` (rnf $ ppuNESStatePaletteMonochrome ppuState)
    `seq` (rnf $ ppuNESStateBackgroundClipped ppuState)
    `seq` (rnf $ ppuNESStateSpritesClipped ppuState)
    `seq` (rnf $ ppuNESStateBackgroundVisible ppuState)
    `seq` (rnf $ ppuNESStateSpritesVisible ppuState)
    `seq` (rnf $ ppuNESStateIntensifiedColor ppuState)
    `seq` (rnf $ ppuNESStateWrittenOddNumberOfTimesToAddresses ppuState)
    `seq` (rnf $ ppuNESStatePermanentAddress ppuState)
    `seq` (rnf $ ppuNESStateTemporaryAddress ppuState)
    `seq` (rnf $ ppuNESStateXOffset ppuState)
    `seq` (rnf $ ppuNESStateIncompleteFrame ppuState)
    `seq` (rnf $ ppuNESStateLatestCompleteFrame ppuState)


instance NFData PrimaryColor where


instance NFData IncompleteVideoFrame where
  rnf incompleteVideoFrame =
    seq (incompleteVideoFrameNameTableMemory incompleteVideoFrame) ()
    -- TODO this is not a deepseq and does not have the desired effect!


instance NFData VideoFrame where
  rnf videoFrame = rnf $ videoFrameNameTable videoFrame


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


registerFetch :: (Monad m)
              => ((Word16 -> m Word8),
                  (Word16 -> Word8 -> m ()),
                  (m (Word16 -> Word8)),
                  (m PPU_NES_State),
                  (PPU_NES_State -> m ()))
              -> Register
              -> m Word8
registerFetch (_, _, _, getState, putState) register = do
  ppuState <- getState
  case register of
    Status -> do
      -- TODO other bits
      let wantsToAssertNMI = ppuNESStateWantsToAssertNMI ppuState
          value = foldl (\value (bitIndex, bitValue) ->
                           if bitValue
                             then setBit value bitIndex
                             else clearBit value bitIndex)
                        0x00
                        [(7, wantsToAssertNMI)]
      putState ppuState {
                   ppuNESStateWantsToAssertNMI = False,
                   ppuNESStateWrittenOddNumberOfTimesToAddresses = False
                 }
      return value
    Access -> do
      return 0x00


registerStore :: (Monad m)
              => ((Word16 -> m Word8),
                  (Word16 -> Word8 -> m ()),
                  (m (Word16 -> Word8)),
                  (m PPU_NES_State),
                  (PPU_NES_State -> m ()))
              -> Register
              -> Word8
              -> m ()
registerStore (_, storeByte, _, getState, putState) register value = do
  ppuState <- getState
  case register of
    Control1 -> do
      let stillPoweringUp = ppuNESStateStillPoweringUp ppuState
      if stillPoweringUp
        then return ()
        else do
          let allowedToAssertNMI = testBit value 7
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
          putState ppuState {
                       ppuNESStateAllowedToAssertNMI = allowedToAssertNMI,
                       ppuNESStateTallSprites = tallSprites,
                       ppuNESStatePatternTableForBackground =
                        patternTableForBackground,
                       ppuNESStatePatternTableForSprites =
                        patternTableForSprites,
                       ppuNESStateAddressIncrementVertically =
                        addressIncrementVertically,
                       ppuNESStatePermanentAddress = permanentAddress'
                     }
    Control2 -> do
      let stillPoweringUp = ppuNESStateStillPoweringUp ppuState
      if stillPoweringUp
        then return ()
        else do
          let paletteMonochrome = testBit value 0
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
          putState ppuState {
                       ppuNESStatePaletteMonochrome = paletteMonochrome,
                       ppuNESStateBackgroundClipped = backgroundClipped,
                       ppuNESStateSpritesClipped = spritesClipped,
                       ppuNESStateBackgroundVisible = backgroundVisible,
                       ppuNESStateSpritesVisible = spritesVisible,
                       ppuNESStateIntensifiedColor = intensifiedColor
                     }
    SpriteAddress -> return ()
    SpriteAccess -> return ()
    Address1 -> do
      let stillPoweringUp = ppuNESStateStillPoweringUp ppuState
      if stillPoweringUp
        then return ()
        else do
          let writtenOddNumberOfTimesToAddresses =
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
          trace ("Write of $" ++ showHexWord8 value ++ " to Address1.")
                $ return ()
          putState ppuState {
                       ppuNESStateWrittenOddNumberOfTimesToAddresses =
                         writtenOddNumberOfTimesToAddresses',
                       ppuNESStatePermanentAddress =
                         permanentAddress',
                       ppuNESStateXOffset = xOffset'
                     }
    Address2 -> do
      let stillPoweringUp = ppuNESStateStillPoweringUp ppuState
      if stillPoweringUp
        then return ()
        else do
          let writtenOddNumberOfTimesToAddresses =
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
          trace ("Write of $" ++ showHexWord8 value ++ " to Address2.")
                $ return ()
          putState ppuState {
                       ppuNESStateWrittenOddNumberOfTimesToAddresses =
                         writtenOddNumberOfTimesToAddresses',
                       ppuNESStatePermanentAddress =
                         permanentAddress',
                       ppuNESStateTemporaryAddress =
                         temporaryAddress'
                     }
    Access -> do
      let addressIncrementVertically =
            ppuNESStateAddressIncrementVertically ppuState
          permanentAddress = ppuNESStatePermanentAddress ppuState
          permanentAddress' = permanentAddress
                              + if addressIncrementVertically
                                  then 32
                                  else 1
      trace ("Write of $" ++ showHexWord8 value ++ " to $"
             ++ showHexWord16 permanentAddress ++ ".")
            $ return ()
      putState ppuState {
                   ppuNESStatePermanentAddress = permanentAddress'
                 }
      storeByte permanentAddress value


assertingNMI :: PPU_NES_State -> Bool
assertingNMI ppuState =
  let wantsToAssertNMI = ppuNESStateWantsToAssertNMI ppuState
      allowedToAssertNMI = ppuNESStateAllowedToAssertNMI ppuState
  in wantsToAssertNMI && allowedToAssertNMI


cycle :: (Monad m)
      => ((Word16 -> m Word8),
          (Word16 -> Word8 -> m ()),
          (m (Word16 -> Word8)),
          (m PPU_NES_State),
          (PPU_NES_State -> m ()))
      -> m ()
{-# INLINE cycle #-}
cycle (fetchByte, storeByte, getTableMemory, getState, putState) = do
  ppuState <- getState
  let horizontalClock = ppuNESStateHorizontalClock ppuState
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
  ppuState <-
    return ppuState {
               ppuNESStateHorizontalClock = horizontalClock',
               ppuNESStateVerticalClock = verticalClock',
               ppuNESStateStillPoweringUp = stillPoweringUp',
               ppuNESStateWantsToAssertNMI = wantsToAssertNMI',
               ppuNESStatePermanentAddress = permanentAddress'
             }
  incompleteFrame <- do
    case (horizontalClock, verticalClock) of
      (0, 240) -> do
        let videoFrame = blankIncompleteVideoFrame
        deepseq videoFrame $ return videoFrame
      (0, 0) -> do
        tableMemory <- getTableMemory
        let incompleteFrame = ppuNESStateIncompleteFrame ppuState
        return incompleteFrame {
                   incompleteVideoFrameNameTableMemory = tableMemory
                 }
      _ -> return $ ppuNESStateIncompleteFrame ppuState
  completeFrame <- do
    case (horizontalClock, verticalClock) of
      (0, 240) -> do
        let videoFrame = computeVideoFrame ppuState
        deepseq videoFrame $ return $ Just videoFrame
      _ -> do
        let maybeVideoFrame = ppuNESStateLatestCompleteFrame ppuState
        deepseq maybeVideoFrame $ return maybeVideoFrame
  putState ppuState {
               ppuNESStateIncompleteFrame = incompleteFrame,
               ppuNESStateLatestCompleteFrame = completeFrame
             }


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
