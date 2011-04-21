module Processor.CPU6502
  (
   CPU6502State(..)
  )
  where

import Data.Word


data CPU6502State =
  CPU6502State {
      cpu6502StateProgramCounter :: Word16,
      cpu6502StateStackPointer :: Word8,
      cpu6502StateAccumulator :: Word8,
      cpu6502StateXIndexRegister :: Word8,
      cpu6502StateYIndexRegister :: Word8,
      cpu6502StateStatusRegister :: Word8
    }
