module FileFormat.INES
  (readINESFile)
  where

import Data.Array.Unboxed
import Data.Bits
import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word
import Prelude hiding (Maybe(..))

import Motherboard.NES
import Data.Strict.Maybe


data INESHeader =
  INESHeaderVersion1 {
      inesHeaderProgramReadOnlyMemoryBlocks :: Word8,
      inesHeaderCharacterReadOnlyMemoryBlocks :: Word8,
      inesHeaderProgramReadWriteMemoryBlocks :: Word8,
      inesHeaderMapperNumber :: Word8,
      inesHeaderMirroringType :: Mirroring,
      inesHeaderBatteryPresent :: Bool,
      inesHeaderTrainerPresent :: Bool,
      inesHeaderSystem :: System
    }


readINESFile :: FilePath -> IO (Maybe HardwareState)
readINESFile filename = do
  bytestring <- BS.readFile filename
  return $ decodeINESFile bytestring


decodeINESHeader :: ByteString -> Maybe (INESHeader, ByteString)
decodeINESHeader bytestring =
  if BS.length bytestring < 16
    then Nothing
    else let (headerBytes, rest) = BS.splitAt 16 bytestring
             magicNumber = BS.take 4 headerBytes
             magicNumberValid = magicNumber == inesHeaderMagicNumber
             versionBits = shiftR (BS.index headerBytes 7) 2 .&. 0x3
             padBytes = BS.take 5 $ BS.drop 11 headerBytes
             padBytesAllZero = all (== 0x00) $ BS.unpack padBytes
             programReadOnlyMemoryBlocks = BS.index headerBytes 4
             characterReadOnlyMemoryBlocks = BS.index headerBytes 5
             programReadWriteMemoryBlocks = BS.index headerBytes 8
             flagsByte1 = BS.index headerBytes 6
             flagsByte2 = BS.index headerBytes 7
             mapperNumberLowNibble = shiftR flagsByte1 4 .&. 0x0F
             mapperNumberHighNibble = shiftR flagsByte2 4 .&. 0x0F
             mapperNumber = shiftL mapperNumberHighNibble 4
                            .|. mapperNumberLowNibble
             mirroringTypeLowBit = shiftR flagsByte1 0 .&. 0x01
             mirroringTypeHighBit = shiftR flagsByte1 3 .&. 0x01
             mirroringType =
               case (mirroringTypeHighBit, mirroringTypeLowBit) of
                 (0, 0) -> HorizontalMirroring
                 (0, 1) -> VerticalMirroring
                 (1, _) -> FourScreenMirroring
             batteryPresentBit = shiftR flagsByte1 1 .&. 0x01
             batteryPresent =
               case batteryPresentBit of
                 0 -> False
                 1 -> True
             trainerPresentBit = shiftR flagsByte1 2 .&. 0x01
             trainerPresent =
               case trainerPresentBit of
                 0 -> False
                 1 -> True
             systemLowBit = shiftR flagsByte2 0 .&. 0x01
             systemHighBit = shiftR flagsByte2 1 .&. 0x01
             (system, systemValid) =
               case (systemHighBit, systemLowBit) of
                 (0, 0) -> (PlainSystem, True)
                 (0, 1) -> (VersusUnisystem, True)
                 (1, 0) -> (PlayChoice10, True)
                 (1, 1) -> (undefined, False)
         in if (not magicNumberValid) || (not systemValid)
              then Nothing
              else if (versionBits == 0) && padBytesAllZero
                     then Just (INESHeaderVersion1 {
                                    inesHeaderProgramReadOnlyMemoryBlocks =
                                      programReadOnlyMemoryBlocks,
                                    inesHeaderCharacterReadOnlyMemoryBlocks =
                                      characterReadOnlyMemoryBlocks,
                                    inesHeaderProgramReadWriteMemoryBlocks =
                                      programReadWriteMemoryBlocks,
                                    inesHeaderMapperNumber =
                                      mapperNumber,
                                    inesHeaderMirroringType =
                                      mirroringType,
                                    inesHeaderBatteryPresent =
                                      batteryPresent,
                                    inesHeaderTrainerPresent =
                                      trainerPresent,
                                    inesHeaderSystem =
                                      system
                                  },
                                rest)
                     else Nothing


decodeINESFile :: ByteString -> Maybe HardwareState
decodeINESFile bytestring = do
  (header, bytestring) <- decodeINESHeader bytestring
  (maybeTrainer, bytestring) <-
    case (inesHeaderTrainerPresent header, BS.length bytestring >= 512) of
      (False, _) -> return $ (Nothing, bytestring)
      (True, True) -> return $ (Just
                                $ array (0, 511)
                                        $ zip [0..]
                                              $ BS.unpack
                                                $ BS.take 512 bytestring,
                                BS.drop 512 bytestring)
      _ -> Nothing
  programReadOnlyMemorySize <-
    return $ (fromIntegral $ inesHeaderProgramReadOnlyMemoryBlocks header)
             * 16384
  (programReadOnlyMemory, bytestring) <-
    if BS.length bytestring >= fromIntegral programReadOnlyMemorySize
      then return $ (array (0, programReadOnlyMemorySize - 1)
                           $ zip [0..]
                                 $ BS.unpack
                                   $ BS.take
                                      (fromIntegral programReadOnlyMemorySize)
                                      bytestring,
                     BS.drop programReadOnlyMemorySize bytestring)
      else Nothing
  characterReadOnlyMemorySize <-
    return $ (fromIntegral $ inesHeaderCharacterReadOnlyMemoryBlocks header)
             * 8192
  (characterReadOnlyMemory, bytestring) <-
    if BS.length bytestring >= characterReadOnlyMemorySize
      then return $ (array (0, characterReadOnlyMemorySize - 1)
                           $ zip [0..]
                                 $ BS.unpack
                                   $ BS.take characterReadOnlyMemorySize
                                             bytestring,
                     BS.drop characterReadOnlyMemorySize bytestring)
      else Nothing
  (maybePlayChoice10HintScreen, bytestring) <-
    case (inesHeaderSystem header == PlayChoice10,
          BS.length bytestring >= 8192) of
      (False, _) -> return $ (Nothing, bytestring)
      (True, True) -> return $ (Just $ array (0, 8191)
                                             $ zip [0..]
                                                   $ BS.unpack
                                                     $ BS.take 8192 bytestring,
                                BS.drop 8192 bytestring)
      _ -> Nothing
  if BS.null bytestring
    then return $ HardwareState {
                      hardwareStateProgramReadOnlyMemory =
                        programReadOnlyMemory,
                      hardwareStateCharacterReadOnlyMemory =
                        characterReadOnlyMemory,
                      hardwareStateTrainer =
                        maybeTrainer,
                      hardwareStatePlayChoice10HintScreen =
                        maybePlayChoice10HintScreen,
                      hardwareStateMapperNumber =
                        inesHeaderMapperNumber header,
                      hardwareStateMirroringType =
                        inesHeaderMirroringType header,
                      hardwareStateBatteryPresent =
                        inesHeaderBatteryPresent header,
                      hardwareStateSystem =
                        inesHeaderSystem header
                    }
    else Nothing


inesHeaderMagicNumber :: ByteString
inesHeaderMagicNumber =
  BS.pack $ map fromIntegral [ord 'N', ord 'E', ord 'S', 0x1A]
