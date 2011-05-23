{-# LANGUAGE BangPatterns, FlexibleInstances #-}
module PPU.PPU_NES
  (
   PPU_NES_State(..),
   Register(..),
   IncompleteVideoFrame(..),
   VideoFrame(..),
   MonadChip(..),
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


class (Monad m) => MonadChip m where
  debugFetchByte :: Word16 -> m Word8
  fetchByte :: Word16 -> m Word8
  storeByte :: Word16 -> Word8 -> m ()
  getTableMemory :: m (Word16 -> Word8)
  getHorizontalClock :: m Int
  putHorizontalClock :: Int -> m ()
  getVerticalClock :: m Int
  putVerticalClock :: Int -> m ()
  getStillPoweringUp :: m Bool
  putStillPoweringUp :: Bool -> m ()
  getWantsToAssertNMI :: m Bool
  putWantsToAssertNMI :: Bool -> m ()
  getAllowedToAssertNMI :: m Bool
  putAllowedToAssertNMI :: Bool -> m ()
  getTallSprites :: m Bool
  putTallSprites :: Bool -> m ()
  getPatternTableForBackground :: m Int
  putPatternTableForBackground :: Int -> m ()
  getPatternTableForSprites :: m Int
  putPatternTableForSprites :: Int -> m ()
  getAddressIncrementVertically :: m Bool
  putAddressIncrementVertically :: Bool -> m ()
  getPaletteMonochrome :: m Bool
  putPaletteMonochrome :: Bool -> m ()
  getBackgroundClipped :: m Bool
  putBackgroundClipped :: Bool -> m ()
  getSpritesClipped :: m Bool
  putSpritesClipped :: Bool -> m ()
  getBackgroundVisible :: m Bool
  putBackgroundVisible :: Bool -> m ()
  getSpritesVisible :: m Bool
  putSpritesVisible :: Bool -> m ()
  getIntensifiedColor :: m (Maybe PrimaryColor)
  putIntensifiedColor :: (Maybe PrimaryColor) -> m ()
  getWrittenOddNumberOfTimesToAddresses :: m Bool
  putWrittenOddNumberOfTimesToAddresses :: Bool -> m ()
  getPermanentAddress :: m Word16
  putPermanentAddress :: Word16 -> m ()
  getTemporaryAddress :: m Word16
  putTemporaryAddress :: Word16 -> m ()
  getXOffset :: m Word8
  putXOffset :: Word8 -> m ()
  getLatestCompleteFrame :: m (Maybe VideoFrame)
  putLatestCompleteFrame :: (Maybe VideoFrame) -> m ()
  getIncompleteVideoFrameNameTableMemory :: m (Word16 -> Word8)
  putIncompleteVideoFrameNameTableMemory :: (Word16 -> Word8) -> m ()


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
    (rnf $ incompleteVideoFrameNameTableMemory incompleteVideoFrame)


instance NFData (Word16 -> Word8) where
  rnf function =
    seq function ()
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
      ppuNESStateIncompleteFrame =
        IncompleteVideoFrame {
            incompleteVideoFrameNameTableMemory =
             blankIncompleteVideoFrameNameTableMemory
          },
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


registerFetch :: (MonadChip m) => Register -> m Word8
registerFetch register = do
  case register of
    Status -> do
      -- TODO other bits
      wantsToAssertNMI <- getWantsToAssertNMI
      let value = foldl (\value (bitIndex, bitValue) ->
                           if bitValue
                             then setBit value bitIndex
                             else clearBit value bitIndex)
                        0x00
                        [(7, wantsToAssertNMI)]
      putWantsToAssertNMI False
      putWrittenOddNumberOfTimesToAddresses False
      return value
    Access -> do
      return 0x00


registerStore :: (MonadChip m) => Register -> Word8 -> m ()
registerStore register value = do
  case register of
    Control1 -> do
      stillPoweringUp <- getStillPoweringUp
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
          permanentAddress <- getPermanentAddress
          let permanentAddress' =
                (permanentAddress .&. 0xF3FF)
                .|. (shiftL (fromIntegral nameTable) 10)
          putAllowedToAssertNMI allowedToAssertNMI
          putTallSprites tallSprites
          putPatternTableForBackground patternTableForBackground
          putPatternTableForSprites patternTableForSprites
          putAddressIncrementVertically addressIncrementVertically
          putPermanentAddress permanentAddress'
    Control2 -> do
      stillPoweringUp <- getStillPoweringUp
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
          putPaletteMonochrome paletteMonochrome
          putBackgroundClipped backgroundClipped
          putSpritesClipped spritesClipped
          putBackgroundVisible backgroundVisible
          putSpritesVisible spritesVisible
          putIntensifiedColor intensifiedColor
    SpriteAddress -> return ()
    SpriteAccess -> return ()
    Address1 -> do
      stillPoweringUp <- getStillPoweringUp
      if stillPoweringUp
        then return ()
        else do
          writtenOddNumberOfTimesToAddresses <-
            getWrittenOddNumberOfTimesToAddresses
          let writtenOddNumberOfTimesToAddresses' =
               not writtenOddNumberOfTimesToAddresses
              valueHighFive = shiftR value 3 .&. 0x1F
              valueLowThree = value .&. 0x07
          permanentAddress <- getPermanentAddress
          xOffset <- getXOffset
          let (permanentAddress', xOffset') =
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
          putWrittenOddNumberOfTimesToAddresses
           writtenOddNumberOfTimesToAddresses'
          putPermanentAddress permanentAddress'
          putXOffset xOffset'
    Address2 -> do
      stillPoweringUp <- getStillPoweringUp
      if stillPoweringUp
        then return ()
        else do
          writtenOddNumberOfTimesToAddresses
            <- getWrittenOddNumberOfTimesToAddresses
          let writtenOddNumberOfTimesToAddresses' =
               not writtenOddNumberOfTimesToAddresses
          permanentAddress <- getPermanentAddress
          temporaryAddress <- getTemporaryAddress
          let (permanentAddress', temporaryAddress') =
                if not writtenOddNumberOfTimesToAddresses
                  then ((permanentAddress .&. 0x00FF)
                        .|. (shiftL (fromIntegral $ value .&. 0x3F) 8),
                        temporaryAddress)
                  else ((permanentAddress .&. 0xFF00)
                        .|. (fromIntegral value),
                        permanentAddress')
          trace ("Write of $" ++ showHexWord8 value ++ " to Address2.")
                $ return ()
          putWrittenOddNumberOfTimesToAddresses
           writtenOddNumberOfTimesToAddresses'
          putPermanentAddress permanentAddress'
          putTemporaryAddress temporaryAddress'
    Access -> do
      addressIncrementVertically <- getAddressIncrementVertically
      permanentAddress <- getPermanentAddress
      let permanentAddress' = permanentAddress
                              + if addressIncrementVertically
                                  then 32
                                  else 1
      trace ("Write of $" ++ showHexWord8 value ++ " to $"
             ++ showHexWord16 permanentAddress ++ ".")
            $ return ()
      putPermanentAddress permanentAddress'
      storeByte permanentAddress value


assertingNMI :: (MonadChip m) => m Bool
assertingNMI = do
  wantsToAssertNMI <- getWantsToAssertNMI
  allowedToAssertNMI <- getAllowedToAssertNMI
  return $ wantsToAssertNMI && allowedToAssertNMI


cycle :: (MonadChip m) => m ()
{-# INLINE cycle #-}
cycle = do
  horizontalClock <- getHorizontalClock
  verticalClock <- getVerticalClock
  let horizontalClock' = mod (horizontalClock + 1) 341
      verticalClock' = if horizontalClock == 340
                          then mod (verticalClock + 1) 262
                          else verticalClock
  stillPoweringUp <- getStillPoweringUp
  let stillPoweringUp' =
        case (stillPoweringUp, horizontalClock, verticalClock) of
          (False, _, _) -> False
          (True, 314, 260) -> False
          (True, _, _) -> True
  wantsToAssertNMI <- getWantsToAssertNMI
  let wantsToAssertNMI' =
        case (wantsToAssertNMI, horizontalClock, verticalClock) of
          (_, 0, 241) -> True
          (_, 0, 260) -> False
          (oldValue, _, _) -> oldValue
  backgroundVisible <- getBackgroundVisible
  spritesVisible <- getSpritesVisible
  let forcedBlank = not backgroundVisible && not spritesVisible
  temporaryAddress <- getTemporaryAddress
  permanentAddress <- getPermanentAddress
  let permanentAddress' =
        if forcedBlank
          then permanentAddress
          else case (horizontalClock, verticalClock) of
                 (0, 261) -> temporaryAddress
                 (0, _) -> (permanentAddress .&. 0xFBE0)
                           .|. (temporaryAddress .&. 0x041F)
                 _ -> permanentAddress
  putHorizontalClock horizontalClock'
  putVerticalClock verticalClock'
  putStillPoweringUp stillPoweringUp'
  putWantsToAssertNMI wantsToAssertNMI'
  putPermanentAddress permanentAddress'
  case (horizontalClock, verticalClock) of
    (0, 240) -> do
      putNewlyComputedVideoFrame
      putBlankIncompleteVideoFrame
    (0, 0) -> do
      tableMemory <- getTableMemory
      putIncompleteVideoFrameNameTableMemory tableMemory
    _ -> return ()


putBlankIncompleteVideoFrame :: (MonadChip m) => m ()
putBlankIncompleteVideoFrame = do
  putIncompleteVideoFrameNameTableMemory
   blankIncompleteVideoFrameNameTableMemory


blankIncompleteVideoFrameNameTableMemory :: Word16 -> Word8
blankIncompleteVideoFrameNameTableMemory = (\_ -> 0x00)


putNewlyComputedVideoFrame :: (MonadChip m) => m ()
putNewlyComputedVideoFrame = do
  nameTableMemory <- getIncompleteVideoFrameNameTableMemory
  putLatestCompleteFrame
   $ Just VideoFrame {
              videoFrameNameTable =
                array ((0, 0), (32, 29))
                      $ map (\index@(x, y) ->
                               let name = nameTableMemory
                                           $ fromIntegral $ y * 32 + x
                               in (index, name))
                            [(x, y) | y <- [0 .. 29], x <- [0 .. 32]]
            }
