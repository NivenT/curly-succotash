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
                                     val     = (.&.) r (rs!!0)
                                 in Left (g', emu{regs=rpl_nth rs 0 val})
  | otherwise = Right $ "Instruction (0x" ++ (showHex op "") ++ ") has not yet been implemented"
    where x = (`shiftR` 2) $ (.&.) op 4
          y = (`shiftR` 1) $ (.&.) op 2
