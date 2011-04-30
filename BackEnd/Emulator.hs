{-# LANGUAGE ForeignFunctionInterface #-}
module Emulator () where

import Data.Array.IArray
import Data.Bits
import Data.Int
import Data.List
import Data.Maybe
import Data.Word
import Foreign
import Foreign.C

import FileFormat.INES
import Motherboard.NES
import PPU.PPU_NES
import Processor.CPU_6502


foreign export ccall "emulator_get_n_textures" getNTextures
    :: IO Word32
foreign export ccall "emulator_get_texture" getTexture
    :: Word32 -> Ptr Word8 -> IO ()


getNTextures :: IO Word32
getNTextures = do
  maybeHardwareState <-
    readINESFile "/Users/dankna/Projects/legal-emulator/Tests/smb1.nes"
  case maybeHardwareState of
    Nothing -> return 0
    Just hardwareState -> do
      let characterReadOnlyMemory =
            hardwareStateCharacterReadOnlyMemory hardwareState
          nBytes = 1 + (fromIntegral $ snd $ bounds characterReadOnlyMemory)
          nBanks = div nBytes 0x2000
          nTextures = 2 * 2 * nBanks
      return nTextures


getTexture :: Word32 -> Ptr Word8 -> IO ()
getTexture textureIndex pointer = do
  maybeHardwareState <- 
    readINESFile "/Users/dankna/Projects/legal-emulator/Tests/smb1.nes"
  case maybeHardwareState of
    Nothing -> return ()
    Just hardwareState -> do
      let characterReadOnlyMemory =
            hardwareStateCharacterReadOnlyMemory hardwareState
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
