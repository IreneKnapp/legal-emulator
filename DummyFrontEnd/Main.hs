{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import Foreign
import Foreign.C


newtype Game = Game (Ptr ())
newtype GameState = GameState (Ptr ())
newtype VideoFrame = VideoFrame (Ptr ())


foreign import ccall "string_free" stringFree
    :: CString -> IO ()
foreign import ccall "emulator_load_game" emulatorLoadGame
    :: CString -> IO Game
foreign import ccall "game_free" gameFree
    :: Game -> IO ()
foreign import ccall "game_name" gameName
    :: Game -> IO CString
foreign import ccall "game_get_n_textures" gameGetNTextures
    :: Game -> IO Word32
foreign import ccall "game_get_texture" gameGetTexture
    :: Game -> Word32 -> Ptr Word8 -> IO ()
foreign import ccall "game_power_on_state" gamePowerOnState
    :: Game -> IO GameState
foreign import ccall "gamestate_free" gamestateFree
    :: GameState -> IO ()
foreign import ccall "gamestate_about_to_begin_instruction"
                     gamestateAboutToBeginInstruction
    :: GameState -> IO CInt
foreign import ccall "gamestate_disassemble_upcoming_instruction"
                     gamestateDisassembleUpcomingInstruction
    :: GameState -> IO CString
foreign import ccall "gamestate_frame_forward" gamestateFrameForward
    :: GameState -> Ptr CString -> IO GameState
foreign import ccall "gamestate_get_video_frame" gamestateGetVideoFrame
    :: GameState -> IO VideoFrame
foreign import ccall "video_frame_free" videoFrameFree
    :: VideoFrame -> IO ()
foreign import ccall "video_frame_get_name_table" videoFrameGetNameTable
    :: VideoFrame -> Ptr Word8 -> IO ()


main :: IO ()
main = do
  game <- withCString "smb1.nes" (\cString -> emulatorLoadGame cString)
  nameCString <- gameName game
  name <- peekCString nameCString
  stringFree nameCString
  putStrLn name
  gamestate <- gamePowerOnState game
  gamestate' <- gamestateFrameForward gamestate nullPtr
  gamestateFree gamestate
  gamestate <- return gamestate'
  nTextures <- gameGetNTextures game
  textureBuffer <- mallocArray 16384
  mapM_ (\i -> do
           gameGetTexture game i textureBuffer)
        [0 .. nTextures - 1]
  free textureBuffer
  nameTableBuffer <- mallocArray $ 33 * 30
  let loop gamestate i = do
        if i == 3
          then return gamestate
          else do
            gamestate' <- gamestateFrameForward gamestate nullPtr
            gamestateFree gamestate
            gamestate <- return gamestate'
            videoFrame <- gamestateGetVideoFrame gamestate
            videoFrameGetNameTable videoFrame nameTableBuffer
            videoFrameFree videoFrame
            loop gamestate (i + 1)
  gamestate <- loop gamestate 0
  free nameTableBuffer
  gamestateFree gamestate
  gameFree game
