module Main (main) where

import Data.Array.IArray

import FileFormat.INES
import Motherboard.NES


main :: IO ()
main = do
  maybeHardwareState <- 
    readINESFile "/Users/dankna/Projects/legal-emulator/Tests/nestest.nes"
  case maybeHardwareState of
    Nothing -> putStrLn $ "Failed to load."
    Just hardwareState -> do
      putStrLn $ "Mapper " ++ (show $ hardwareStateMapperNumber hardwareState)
      putStrLn $ "PRG ROM "
                 ++ (show
                     $ (1+)
                     $ snd
                     $ bounds
                     $ hardwareStateProgramReadOnlyMemory hardwareState)
      putStrLn $ "CHR ROM "
                 ++ (show
                     $ (1+)
                     $ snd
                     $ bounds
                     $ hardwareStateCharacterReadOnlyMemory hardwareState)
