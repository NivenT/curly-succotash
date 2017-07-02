module Op (
  exec_op,
  World
) where 

import System.Random
import Data.Bits
import Data.Word
import Numeric

import Emulator

type World g = (g, Chip8)

rand_byte :: (RandomGen g) => g -> (Word8, g)
rand_byte = random

rpl_nth :: [a] -> Int -> a -> [a]
rpl_nth (x:xs) 0 y = y:xs
rpl_nth (x:xs) n y = y:rpl_nth xs (n-1) y

exec_op :: (RandomGen g) => Chip8 -> Int -> g -> Either (World g) String
exec_op emu op rng
  | op `elem` [0xb000..0xbfff] = let n = (.&.) op 7
                                     p = pc emu
                                 in Left (rng, emu{pc=p+n-2})
  | op `elem` [0xc000..0xcfff] = let (r, g') = rand_byte rng
                                     rs      = regs emu
                                     n       = fromIntegral $ (.&.) op 3
                                     val     = (.&.) r n
                                 in Left (g', emu{regs=rpl_nth rs x val})
  | otherwise = Right $ "Instruction (0x" ++ (showHex op "") ++ ") has not yet been implemented (X = " ++ (showHex x "") ++ " | Y = " ++ (showHex y "") ++ ")" 
    where x = (`shiftR` 8) $ (.&.) op 0x0F00
          y = (`shiftR` 4) $ (.&.) op 0x00F0
