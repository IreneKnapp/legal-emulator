module Assembly
  (showHexWord8,
   showHexWord16,
   leftPad,
   rightPad,
   padding)
  where

import Data.Bits
import Data.Word


showHexWord16 :: Word16 -> String
showHexWord16 word =
  let highByte = fromIntegral $ (shiftR word 8) .&. 0xFF
      lowByte = fromIntegral $ (shiftR word 0) .&. 0xFF
  in (showHexWord8 highByte) ++ (showHexWord8 lowByte)


showHexWord8 :: Word8 -> String
showHexWord8 byte =
  let highNibble = (shiftR byte 4) .&. 0xF
      lowNibble = (shiftR byte 0) .&. 0xF
      showHexNibble nibble =
        [(['0' .. '9'] ++ ['A' .. 'F']) !! fromIntegral nibble]
  in (showHexNibble highNibble) ++ (showHexNibble lowNibble)


leftPad :: String -> Int -> String
leftPad string width = padding (width - length string) ++ string


rightPad :: String -> Int -> String
rightPad string width = string ++ padding (width - length string)


padding :: Int -> String
padding width = take width $ repeat ' '
