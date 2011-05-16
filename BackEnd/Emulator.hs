{-# LANGUAGE ForeignFunctionInterface, BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Emulator () where

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

import Control.DeepSeq
import Data.Array.Unboxed



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
  return $ if NES.aboutToBeginInstruction state
             then 1
             else 0


gamestateDisassembleUpcomingInstruction :: StablePtr NES.State -> IO CString
gamestateDisassembleUpcomingInstruction state = do
  state <- deRefStablePtr state
  let disassembly = NES.disassembleUpcomingInstruction state
  stringNew disassembly


gamestateFrameForward
    :: StablePtr NES.State -> Ptr CString -> IO (StablePtr NES.State)
gamestateFrameForward state tracePointer = do
  state <- deRefStablePtr state
  let loop vblankEnded !traceLines !state = do
        let !traceLines' =
              if tracePointer /= nullPtr
                then if NES.aboutToBeginInstruction state
                       then traceLines
                            ++ [NES.disassembleUpcomingInstruction state]
                       else traceLines
                else []
            softwareState = NES.stateSoftwareState state
            motherboardClock =
              NES.softwareStateMotherboardClockCount softwareState
            ppuState = NES.softwareStatePPUState softwareState
            horizontalClock = PPU.ppuNESStateHorizontalClock ppuState
            verticalClock = PPU.ppuNESStateVerticalClock ppuState
            vblankEnded' =
              vblankEnded
              || ((horizontalClock == 0) && (verticalClock == 261))
            ppuEligibleToEnd =
              vblankEnded
              && (verticalClock >= 240)
              && (verticalClock < 261)
              && (isJust $ PPU.ppuNESStateLatestCompleteFrame ppuState)
            cpuState = NES.softwareStateCPUState softwareState
            cpuEligibleToEnd = CPU.atInstructionStart cpuState
            motherboardEligibleToEnd = NES.atCPUCycle state
            shouldEnd =
              ppuEligibleToEnd
              && cpuEligibleToEnd
              && motherboardEligibleToEnd
        if shouldEnd
          then do
            if tracePointer /= nullPtr
              then do
                let trace = concat $ map (\line -> line ++ "\n") traceLines
                traceCString <- stringNew trace
                poke tracePointer traceCString
              else return ()
            newStablePtr state
          else deepseq state $ loop vblankEnded' traceLines' $ NES.cycle state
  loop False [] state


instance NFData NES.State where
  rnf state =
    (rnf $ NES.stateHardwareState state)
    `seq` (rnf $ NES.stateSoftwareState state)


instance NFData NES.HardwareState where
  rnf hardwareState =
    (rnf $ NES.hardwareStateProgramReadOnlyMemory hardwareState)
    `seq` (rnf $ NES.hardwareStateCharacterReadOnlyMemory hardwareState)
    `seq` (rnf $ NES.hardwareStateTrainer hardwareState)
    `seq` (rnf $ NES.hardwareStatePlayChoice10HintScreen hardwareState)
    `seq` (rnf $ NES.hardwareStateMapperNumber hardwareState)
    `seq` (rnf $ NES.hardwareStateMirroringType hardwareState)
    `seq` (rnf $ NES.hardwareStateBatteryPresent hardwareState)
    `seq` (rnf $ NES.hardwareStateSystem hardwareState)


instance NFData NES.SoftwareState where
  rnf softwareState =
    (rnf $ NES.softwareStateMotherboardClockCount softwareState)
    `seq` (rnf $ NES.softwareStateLastCPUDataBusValue softwareState)
    `seq` (rnf $ NES.softwareStateLastPPUDataBusValue softwareState)
    `seq` (rnf $ NES.softwareStateCPUState softwareState)
    `seq` (rnf $ NES.softwareStatePPUState softwareState)
    `seq` (rnf $ NES.softwareStateMotherboardCPUMemory softwareState)
    `seq` (rnf $ NES.softwareStateMotherboardPPUTableMemory softwareState)
    `seq` (rnf $ NES.softwareStateMotherboardPPUPaletteMemory softwareState)
    `seq` (rnf $ NES.softwareStateMotherboardPPUSpriteMemory softwareState)


instance NFData NES.Mirroring where


instance NFData NES.System where


instance NFData CPU.CPU_6502_State where
  rnf cpuState =
    (rnf $ CPU.cpu6502StateProgramCounter cpuState)
    `seq` (rnf $ CPU.cpu6502StateStackPointer cpuState)
    `seq` (rnf $ CPU.cpu6502StateAccumulator cpuState)
    `seq` (rnf $ CPU.cpu6502StateXIndexRegister cpuState)
    `seq` (rnf $ CPU.cpu6502StateYIndexRegister cpuState)
    `seq` (rnf $ CPU.cpu6502StateStatusRegister cpuState)
    `seq` (rnf $ CPU.cpu6502StateInternalOverflow cpuState)
    `seq` (rnf $ CPU.cpu6502StateInternalNegative cpuState)
    `seq` (rnf $ CPU.cpu6502StateInternalStoredAddress cpuState)
    `seq` (rnf $ CPU.cpu6502StateInternalLatch cpuState)
    `seq` (rnf $ CPU.cpu6502StateMicrocodeInstructionQueue cpuState)
    `seq` (rnf $ CPU.cpu6502StateInterruptNoticed cpuState)
    `seq` (rnf $ CPU.cpu6502StateInterruptAlreadyProcessed cpuState)
    `seq` (rnf $ CPU.cpu6502StateNonMaskableInterruptAlreadyProcessed cpuState)


instance NFData CPU.MicrocodeInstruction where
  rnf microcodeInstruction =
    (rnf $ CPU.microcodeInstructionConditional microcodeInstruction)
    `seq`
      (rnf $ CPU.microcodeInstructionInsteadFixProgramCounterHighByteIfNecessary
       microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionRegister microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionRegisterFromLatch microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionAddLatchToProgramCounterLowByte
           microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionAddressSource microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionAddressOffset microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionAddressAddOne microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionSettingStoredValueBits microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionClearingFetchedValueBits microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionReadWrite microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionArithmeticOperation microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionDecodeOperation microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionIncrementProgramCounter microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionZeroStoredAddressHighByte microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionAddRegisterToStoredAddress
           microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionFixStoredAddressHighByte microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionStackPointerOperation microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionXIndexRegisterOperation microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionYIndexRegisterOperation microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionStatusRegisterOperation microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionAccumulatorOperation microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionLatchOperation microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionRegisterRegisterCopy microcodeInstruction)
    `seq` (rnf $ CPU.microcodeInstructionUpdateStatusForRegister microcodeInstruction)


instance NFData CPU.InternalRegister where


instance NFData CPU.InterruptType where


instance NFData CPU.Condition where


instance NFData CPU.ReadWrite where


instance NFData CPU.ArithmeticOperation where


instance NFData CPU.IncrementDecrement where


instance NFData CPU.SetClear where


instance NFData CPU.Transformation where
  rnf CPU.ArithmeticShiftLeft = ()
  rnf CPU.LogicalShiftRight = ()
  rnf CPU.RotateLeft = ()
  rnf CPU.RotateRight = ()
  rnf (CPU.IncrementDecrement incrementDecrement) =
    rnf incrementDecrement


instance NFData CPU.AddressSource where
  rnf CPU.ProgramCounterAddressSource = ()
  rnf (CPU.FixedAddressSource word) = rnf word
  rnf CPU.StoredAddressSource = ()



instance NFData PPU.PPU_NES_State where
  rnf ppuState =
    (rnf $ PPU.ppuNESStateHorizontalClock ppuState)
    `seq` (rnf $ PPU.ppuNESStateVerticalClock ppuState)
    `seq` (rnf $ PPU.ppuNESStateStillPoweringUp ppuState)
    `seq` (rnf $ PPU.ppuNESStateWantsToAssertNMI ppuState)
    `seq` (rnf $ PPU.ppuNESStateAllowedToAssertNMI ppuState)
    `seq` (rnf $ PPU.ppuNESStateTallSprites ppuState)
    `seq` (rnf $ PPU.ppuNESStatePatternTableForBackground ppuState)
    `seq` (rnf $ PPU.ppuNESStatePatternTableForSprites ppuState)
    `seq` (rnf $ PPU.ppuNESStateAddressIncrementVertically ppuState)
    `seq` (rnf $ PPU.ppuNESStatePaletteMonochrome ppuState)
    `seq` (rnf $ PPU.ppuNESStateBackgroundClipped ppuState)
    `seq` (rnf $ PPU.ppuNESStateSpritesClipped ppuState)
    `seq` (rnf $ PPU.ppuNESStateBackgroundVisible ppuState)
    `seq` (rnf $ PPU.ppuNESStateSpritesVisible ppuState)
    `seq` (rnf $ PPU.ppuNESStateIntensifiedColor ppuState)
    `seq` (rnf $ PPU.ppuNESStateWrittenOddNumberOfTimesToAddresses ppuState)
    `seq` (rnf $ PPU.ppuNESStatePermanentAddress ppuState)
    `seq` (rnf $ PPU.ppuNESStateTemporaryAddress ppuState)
    `seq` (rnf $ PPU.ppuNESStateXOffset ppuState)
    `seq` (rnf $ PPU.ppuNESStateIncompleteFrame ppuState)
    `seq` (rnf $ PPU.ppuNESStateLatestCompleteFrame ppuState)


instance NFData PPU.PrimaryColor where


instance NFData PPU.IncompleteVideoFrame where
  rnf incompleteVideoFrame =
    seq (PPU.incompleteVideoFrameNameTableMemory incompleteVideoFrame) ()
    -- TODO this is not a deepseq and does not have the desired effect!


instance NFData PPU.VideoFrame where
  rnf videoFrame = rnf $ PPU.videoFrameNameTable videoFrame


instance (NFData i, NFData v, Ix i, IArray UArray v)
         => NFData (UArray i v) where
         {-
  rnf a = rnf $ elems a-}


instance (NFData a) => NFData (Maybe a) where
  rnf Nothing = ()
  rnf (Just something) = rnf something


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
