module Op (
  exec_op,
  World
) where 

import System.Random
import Data.Bits
import Data.Word
import Data.List.Split
import Numeric

import Emulator

type World g = (g, Chip8)

rand_byte :: (RandomGen g) => g -> (Word8, g)
rand_byte = random

rpl_nth :: [a] -> Int -> a -> [a]
rpl_nth (x:xs) 0 y = y:xs
rpl_nth (x:xs) n y = y:rpl_nth xs (n-1) y

flatten :: [[a]] -> [a]
flatten (x:[]) = x
flatten (x:xs) = x ++ flatten xs

-- There's probably a less trash way to do this
draw_sprite :: Chip8 -> Int -> Int -> Int -> Chip8
draw_sprite emu x y h = emu{screen=chunksOf 32 s', regs=rpl_nth rs 0xf (if flag then 1 else 0)}
  where m = mem emu
        rs = regs emu
        vx = rs!!x
        vy = rs!!y
        p = ptr emu
        indices = flatten $ map (\r -> map (\c -> (r, c)) [0..8]) [0..h]
        s = flatten . map (\(i, vals) -> map (\(j, val) -> if elem (i, j) indices then f i j val else (val, False)) vals) . map (\(i, row) -> (i, zip [0..] row)) . zip [0..] $ screen emu
        (s', flag) = (map (\(v, b) -> v) s, any (\(v, b) -> b) s)
        f i j v = let x = mod (vx+fromIntegral j) 64
                      y = mod (vy+fromIntegral i) 32
                      bit = 0 /= ((.&.) (m!!(p+fromIntegral i)) (shiftR 128 j))
                  in (xor v bit, bit && (bit == v))

-- Todo: Replace `elem` with in_range function
exec_op :: (RandomGen g) => Chip8 -> Int -> g -> Either (World g) String
exec_op emu op rng
  | op `elem` [0x6000..0x6fff] = let n = fromIntegral $ (.&.) op 0x00ff
                                 in Left (rng, emu{regs=rpl_nth rs x n})
  | op `elem` [0x7000..0x7fff] = let n = fromIntegral $ (.&.) op 0x00ff
                                 in Left (rng, emu{regs=rpl_nth rs x (vx+n)})
  | op `elem` [0xb000..0xbfff] = let n = (.&.) op 0x0fff
                                 in Left (rng, emu{pc=p+n-2})
  | op `elem` [0xc000..0xcfff] = let (r, g') = rand_byte rng
                                     n       = fromIntegral $ (.&.) op 0x00ff
                                     val     = (.&.) r n
                                 in Left (g', emu{regs=rpl_nth rs x val})
  | op `elem` [0xd000..0xdfff] = Left (rng, draw_sprite emu x y $ (.&.) op 4)
  | ((.&.) op 0xf000) == 0xe && ((.&.) op 0x00ff) == 0x9e = if ks!!(fromIntegral . toInteger $ vx)
                                                               then Left (rng, emu{pc=p+2})
                                                               else Left (rng, emu)
  | ((.&.) op 0xf000) == 0xe && ((.&.) op 0x00ff) == 0xa1 = if ks!!(fromIntegral . toInteger $ vx)
                                                               then Left (rng, emu)
                                                               else Left (rng, emu{pc=p+2})
  
  | otherwise = Right $ "Instruction (0x" ++ (showHex op "") ++ ") has not yet been implemented (X = " ++ (showHex x "") ++ " | Y = " ++ (showHex y "") ++ ")" 
    where x = (`shiftR` 8) $ (.&.) op 0x0F00
          y = (`shiftR` 4) $ (.&.) op 0x00F0
          ks = keys emu
          rs = regs emu
          vx = rs!!x
          vy = rs!!y
          p = pc emu
