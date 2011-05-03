{-# LANGUAGE ForeignFunctionInterface #-}
module Emulator () where

import Data.Array.IArray
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Foreign
import Foreign.C

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
foreign export ccall "gamestate_frame_forward" gamestateFrameForward
    :: StablePtr NES.State -> IO (StablePtr NES.State)


stringNew :: String -> IO CString
stringNew string = do
  BS.useAsCString (UTF8.fromString string)
                  (\cStringAutomatic -> do
                     let getBufferLength = do
                           getBufferLength' 0 cStringAutomatic
                         getBufferLength' lengthSoFar buffer = do
                           character <- peek buffer
                           case character of
                             0 -> return $ lengthSoFar + 1
                             _ -> getBufferLength' (lengthSoFar + 1)
                                                   (plusPtr buffer 1)
                     bufferLength <- getBufferLength
                     cString <- mallocArray bufferLength
                     copyArray cString cStringAutomatic bufferLength
                     return cString)


stringFree :: CString -> IO ()
stringFree cString = free cString


emulatorLoadGame :: CString -> IO (StablePtr NES.HardwareState)
emulatorLoadGame filename = do
  filename <- BS.packCString filename >>= return . UTF8.toString
  maybeHardwareState <-
    INES.readINESFile filename
  case maybeHardwareState of
    Nothing -> return $ castPtrToStablePtr nullPtr
    Just hardwareState -> newStablePtr hardwareState


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
  newStablePtr state


gamestateFree :: StablePtr NES.State -> IO ()
gamestateFree state = freeStablePtr state


gamestateFrameForward :: StablePtr NES.State -> IO (StablePtr NES.State)
gamestateFrameForward state = do
  state <- deRefStablePtr state
  let loop vblankEnded state = do
        let softwareState = NES.stateSoftwareState state
            motherboardClock =
              NES.softwareStateMotherboardClockCount softwareState
            ppuState = NES.softwareStatePPUState softwareState
            horizontalClock = PPU.ppuNESStateHorizontalClock ppuState
            verticalClock = PPU.ppuNESStateVerticalClock ppuState
            vblankEnded' =
              vblankEnded
              || ((horizontalClock == 0) && (verticalClock == 261))
            ppuEligibleToEnd = vblankEnded && (verticalClock >= 240)
            cpuState = NES.softwareStateCPUState softwareState
            cpuEligibleToEnd = CPU.atInstructionStart cpuState
            motherboardEligibleToEnd = NES.atCPUCycle state
            shouldEnd =
              ppuEligibleToEnd
              && cpuEligibleToEnd
              && motherboardEligibleToEnd
        if shouldEnd
          then newStablePtr state
          else loop vblankEnded' $ NES.cycle state
  loop False state
