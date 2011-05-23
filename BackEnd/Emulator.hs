{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
module Emulator () where

import Control.DeepSeq
import Data.Array.IArray
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Int
import Data.List
import Data.Word
import Foreign
import Foreign.C
import Prelude hiding (Maybe(..))

import Data.Strict.Maybe
import qualified FileFormat.INES as INES
import qualified Motherboard.NES as NES
import qualified PPU.PPU_NES as PPU
import qualified Processor.CPU_6502 as CPU


foreign export ccall "string_free" stringFree
    :: CString -> IO ()
foreign export ccall "emulator_load_game" emulatorLoadGame
    :: CString -> IO (StablePtr NES.HardwareState)
foreign export ccall "game_free" gameFree
    :: StablePtr NES.HardwareState -> IO ()
foreign export ccall "game_name" gameName
    :: StablePtr NES.HardwareState -> IO CString
foreign export ccall "game_get_n_textures" gameGetNTextures
    :: StablePtr NES.HardwareState -> IO Word32
foreign export ccall "game_get_texture" gameGetTexture
    :: StablePtr NES.HardwareState -> Word32 -> Ptr Word8 -> IO ()
foreign export ccall "game_power_on_state" gamePowerOnState
    :: StablePtr NES.HardwareState -> IO (StablePtr NES.State)
foreign export ccall "gamestate_free" gamestateFree
    :: StablePtr NES.State -> IO ()
foreign export ccall "gamestate_about_to_begin_instruction"
                     gamestateAboutToBeginInstruction
    :: StablePtr NES.State -> IO CInt
foreign export ccall "gamestate_disassemble_upcoming_instruction"
                     gamestateDisassembleUpcomingInstruction
    :: StablePtr NES.State -> IO CString
foreign export ccall "gamestate_frame_forward" gamestateFrameForward
    :: StablePtr NES.State -> Ptr CString -> IO (StablePtr NES.State)
foreign export ccall "gamestate_get_video_frame" gamestateGetVideoFrame
    :: StablePtr NES.State -> IO (StablePtr PPU.VideoFrame)
foreign export ccall "video_frame_free" videoFrameFree
    :: StablePtr PPU.VideoFrame -> IO ()
foreign export ccall "video_frame_get_name_table" videoFrameGetNameTable
    :: StablePtr PPU.VideoFrame -> Ptr Word8 -> IO ()


stringNew :: String -> IO CString
stringNew string = do
  let byteString = UTF8.fromString string
      bufferLength = 1 + BS.length byteString
  cString <- mallocArray bufferLength
  mapM_ (\index -> do
           let byte = BS.index byteString index
               element = advancePtr cString index
           poke element byte)
        [0 .. bufferLength - 2]
  poke (advancePtr cString $ bufferLength - 1) 0x00
  return $ castPtr cString


stringFree :: CString -> IO ()
stringFree cString = free cString


emulatorLoadGame :: CString -> IO (StablePtr NES.HardwareState)
emulatorLoadGame filename = do
  filename <- BS.packCString filename >>= return . UTF8.toString
  maybeHardwareState <-
    INES.readINESFile filename
  case maybeHardwareState of
    Nothing -> return $ castPtrToStablePtr nullPtr
    Just hardwareState -> deepseq hardwareState $ newStablePtr hardwareState


gameFree :: StablePtr NES.HardwareState -> IO ()
gameFree hardwareState = freeStablePtr hardwareState


gameName :: StablePtr NES.HardwareState -> IO CString
gameName _ = stringNew "Test ROM"


gameGetNTextures :: StablePtr NES.HardwareState -> IO Word32
gameGetNTextures hardwareState = do
  hardwareState <- deRefStablePtr hardwareState
  let characterReadOnlyMemory =
        NES.hardwareStateCharacterReadOnlyMemory hardwareState
      nBytes = 1 + (fromIntegral $ snd $ bounds characterReadOnlyMemory)
      nBanks = div nBytes 0x2000
      nTextures = 2 * 2 * nBanks
  return nTextures


gameGetTexture :: StablePtr NES.HardwareState -> Word32 -> Ptr Word8 -> IO ()
gameGetTexture hardwareState textureIndex pointer = do
  hardwareState <- deRefStablePtr hardwareState
  let characterReadOnlyMemory =
        NES.hardwareStateCharacterReadOnlyMemory hardwareState
      baseIndex = (div textureIndex 2) * 0x1000
      bitplaneIndex = fromIntegral $ mod textureIndex 2
  pokeArray pointer
            $ concat
               $ map (\pixelRow ->
                        let spriteRow = div pixelRow 8
                            spritePixelRow = mod pixelRow 8
                        in concat
                            $ map (\spriteColumn ->
                                     let spriteIndex =
                                           spriteRow * 16 + spriteColumn
                                         byteIndex =
                                           baseIndex
                                           + spriteIndex * 16
                                           + spritePixelRow
                                           + bitplaneIndex * 8
                                         byte =
                                           characterReadOnlyMemory
                                           ! fromIntegral byteIndex
                                         pixels =
                                           map (\bit ->
                                                  if testBit byte bit
                                                    then 0xFF
                                                    else 0x00)
                                               $ reverse [0 .. 7]
                                     in pixels)
                                  [0 .. 15])
                     $ [0 .. 127]
  return ()


gamePowerOnState :: StablePtr NES.HardwareState -> IO (StablePtr NES.State)
gamePowerOnState hardwareState = do
  hardwareState <- deRefStablePtr hardwareState
  let softwareState = NES.powerOnSoftwareState
      state = NES.State {
                  NES.stateHardwareState = hardwareState,
                  NES.stateSoftwareState = softwareState
                }
  deepseq state $ newStablePtr state


gamestateFree :: StablePtr NES.State -> IO ()
gamestateFree state = freeStablePtr state


gamestateAboutToBeginInstruction :: StablePtr NES.State -> IO CInt
gamestateAboutToBeginInstruction state = do
  state <- deRefStablePtr state
  let (result, _) = NES.runMonadicState NES.getAboutToBeginInstruction state
  return $ if result
             then 1
             else 0


gamestateDisassembleUpcomingInstruction :: StablePtr NES.State -> IO CString
gamestateDisassembleUpcomingInstruction state = do
  state <- deRefStablePtr state
  let (disassembly, _) =
        NES.runMonadicState NES.disassembleUpcomingInstruction state
  stringNew disassembly


gamestateFrameForward
    :: StablePtr NES.State -> Ptr CString -> IO (StablePtr NES.State)
gamestateFrameForward state tracePointer = do
  state <- deRefStablePtr state
  let loop vblankEnded !traceLines = do
        aboutToBeginInstruction <- NES.getAboutToBeginInstruction
        atCPUCycle <- NES.getAtCPUCycle
        motherboardClock <- NES.getSoftwareStateMotherboardClockCount
        horizontalClock <- NES.getSoftwareStatePPUStateHorizontalClock
        verticalClock <- NES.getSoftwareStatePPUStateVerticalClock
        maybeCompleteFrame <- NES.getSoftwareStatePPUStateLatestCompleteFrame
        atInstructionStart <- NES.runCPU CPU.getAtInstructionStart
        let vblankEnded' =
              vblankEnded
              || ((horizontalClock == 0) && (verticalClock == 261))
            ppuEligibleToEnd =
              vblankEnded
              && (verticalClock >= 240)
              && (verticalClock < 261)
              && (isJust maybeCompleteFrame)
            cpuEligibleToEnd = atInstructionStart
            motherboardEligibleToEnd = atCPUCycle
            shouldEnd =
              ppuEligibleToEnd
              && cpuEligibleToEnd
              && motherboardEligibleToEnd
        if shouldEnd
          then return traceLines
          else do
            traceLines <-
              if tracePointer /= nullPtr
                then if aboutToBeginInstruction
                       then do
                         thisLine <- NES.disassembleUpcomingInstruction
                         return $ traceLines ++ [thisLine]
                       else return traceLines
                else return []
            NES.cycle
            loop vblankEnded' traceLines
  let (traceLines, state') = NES.runMonadicState (loop False []) state
  if tracePointer /= nullPtr
    then do
      let trace = concat $ map (\line -> line ++ "\n") traceLines
      traceCString <- stringNew trace
      poke tracePointer traceCString
    else return ()
  deepseq state' $ newStablePtr state'


gamestateGetVideoFrame :: StablePtr NES.State -> IO (StablePtr PPU.VideoFrame)
gamestateGetVideoFrame state = do
  state <- deRefStablePtr state
  let softwareState = NES.stateSoftwareState state
      ppuState = NES.softwareStatePPUState softwareState
      maybeVideoFrame = PPU.ppuNESStateLatestCompleteFrame ppuState
  case maybeVideoFrame of
    Nothing -> return $ castPtrToStablePtr nullPtr
    Just videoFrame -> newStablePtr videoFrame


videoFrameFree :: StablePtr PPU.VideoFrame -> IO ()
videoFrameFree videoFrame = freeStablePtr videoFrame


videoFrameGetNameTable :: StablePtr PPU.VideoFrame -> Ptr Word8 -> IO ()
videoFrameGetNameTable videoFrame buffer = do
  videoFrame <- deRefStablePtr videoFrame
  let nameTable = PPU.videoFrameNameTable videoFrame
  mapM_ (\y -> do
           let bufferRow = advancePtr buffer $ y * 33
           mapM_ (\x -> do
                    let bufferCell = advancePtr bufferRow x
                        value = nameTable ! (x, y)
                    poke bufferCell value)
                 [0 .. 32])
        [0 .. 29]
