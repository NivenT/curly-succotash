module Op (
  exec_op
) where 

import Data.Bits
import Numeric

import Emulator

exec_op :: Chip8 -> Int -> Either Chip8 String
exec_op emu op
  | otherwise = Right $ "Instruction (0x" ++ (showHex op "") ++ ") has not yet been implemented"
    where x = (`shiftR` 2) $ (.&.) op 4
          y = (`shiftR` 1) $ (.&.) op 2
          n = (.&.) op 7
