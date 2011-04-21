module Main (main) where

import FileFormat.INES


main :: IO ()
main = do
  readINESFile "/Users/dankna/Projects/legal-emulator/Tests/nestest.nes"
  putStrLn $ "Foo."
