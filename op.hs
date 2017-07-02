module Op (
  exec_op,
  World
) where 

import System.Random
import Data.Bits
import Data.Word
import Data.List.Split
import Debug.Trace
import Numeric

import Emulator

type World g = (g, Chip8)

rand_byte :: (RandomGen g) => g -> (Word8, g)
rand_byte = random

rpl_nth :: [a] -> Int -> a -> [a]
rpl_nth (x:xs) 0 y = y:xs
rpl_nth (x:xs) n y = x:rpl_nth xs (n-1) y

rpl :: [a] -> [(Int, a)] -> [a]
rpl lst [] = lst
rpl lst ((i, z):zs) = rpl_nth (rpl lst zs) i z
  
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

in_range :: Int -> Int -> Int -> Bool
in_range val min len = min <= val && val < min + len

digits :: Integral a => a -> [a]
digits x = (mod x 10):(digits $ div x 10)

get :: [a] -> Int -> Int -> [a]
get lst start len = take len $ drop start lst

alu :: Chip8 -> Int -> Word8 -> Word8 -> Int -> Chip8
alu emu x vx vy op
  | op == 0   = emu{regs=rpl_nth rs x vy}
  | op == 1   = emu{regs=rpl_nth rs x $ (.|.) vx vy}
  | op == 2   = emu{regs=rpl_nth rs x $ (.&.) vx vy}
  | op == 3   = emu{regs=rpl_nth rs x $ xor vx vy}
  | op == 4   = emu{regs=rpl_nth rs x $ vx + vy}
  | op == 5   = emu{regs=rpl_nth rs x $ vx - vy}
  | op == 6   = emu{regs=rpl_nth rs x $ shiftR vx 1}
  | op == 7   = emu{regs=rpl_nth rs x $ vy - vx}
  | op == 0xe = emu{regs=rpl_nth rs x $ shiftL vx 1}
    where rs = regs emu

-- Todo: Replace `elem` with in_range function
exec_op :: (RandomGen g) => Chip8 -> Int -> g -> Either (World g) String
exec_op emu op rng
  -- | trace ("Running instruction 0x" ++ (showHex op " ") {- ++ (disp_state emu) -}) False = undefined
  | op == 0x00e0               = Left (rng, emu{screen = screen init_emu})
  | op == 0x00ee               = Left (rng, emu{pc=ss!!(s-1), sp=s-1})
  | in_range op 0x1000 0x1000  = let n = fromIntegral $ (.&.) op 0x0fff
                                 in Left (rng, emu{pc=n-2})
  | in_range op 0x2000 0x1000  = let n = fromIntegral $ (.&.) op 0x0fff
                                 in Left (rng, emu{pc=n-2, sp=s+1, stack=rpl_nth ss s p})
  | in_range op 0x3000 0x1000  = let n = fromIntegral $ (.&.) op 0x00ff
                                 in if n == vx
                                       then Left (rng, emu{pc=p+2})
                                       else Left (rng, emu)
  | in_range op 0x4000 0x1000  = let n = fromIntegral $ (.&.) op 0x00ff
                                 in if n == vx
                                       then Left (rng, emu)
                                       else Left (rng, emu{pc=p+2})
  | in_range op 0x6000 0x1000  = let n = fromIntegral $ (.&.) op 0x00ff
                                 in Left (rng, emu{regs=rpl_nth rs x n})
  | in_range op 0x7000 0x1000  = let n = fromIntegral $ (.&.) op 0x00ff
                                 in Left (rng, emu{regs=rpl_nth rs x (vx+n)})
  | in_range op 0x8000 0x1000  = Left(rng, alu emu x vx vy $ (.&.) op 0x000f)
  | in_range op 0xa000 0x1000  = let n = fromIntegral $ (.&.) op 0x0fff
                                 in Left (rng, emu{ptr=n})
  | in_range op 0xb000 0x1000  = let n = (.&.) op 0x0fff
                                 in Left (rng, emu{pc=p+n-2})
  | in_range op 0xc000 0x1000  = let (r, g') = rand_byte rng
                                     n       = fromIntegral $ (.&.) op 0x00ff
                                     val     = (.&.) r n
                                 in Left (g', emu{regs=rpl_nth rs x val})
  | in_range op 0xd000 0x1000  = Left (rng, draw_sprite emu x y $ (.&.) op 0x000f)
  | (.&.) op 0xf0ff == 0xe09e  = if ks!!(fromIntegral . toInteger $ vx)
                                    then Left (rng, emu{pc=p+2})
                                    else Left (rng, emu)
  | (.&.) op 0xf0ff == 0xe0a1  = if ks!!(fromIntegral . toInteger $ vx)
                                    then Left (rng, emu)
                                    else Left (rng, emu{pc=p+2})
  | (.&.) op 0xf0ff == 0xf007  = Left (rng, emu{regs=rpl_nth rs x $ fromIntegral dt})
  | (.&.) op 0xf0ff == 0xf015  = Left (rng, emu{delay_timer=fromIntegral vx})
  | (.&.) op 0xf0ff == 0xf018  = Left (rng, emu{sound_timer=fromIntegral vx})
  | (.&.) op 0xf0ff == 0xf029  = Left (rng, emu{ptr=5 * fromIntegral vx})
  | (.&.) op 0xf0ff == 0xf033  = Left (rng, emu{mem=rpl m . zip [p'+2, p'+1, p'] $ digits vx})
  | (.&.) op 0xf0ff == 0xf065  = Left (rng, emu{regs=rpl rs . zip [0..x] $ get m p' x})
  | otherwise = Right $ "Instruction (0x" ++ (showHex op "") ++ ") has not yet been implemented (X = " ++ (showHex x "") ++ " | Y = " ++ (showHex y "") ++ ")" 
    where x = (`shiftR` 8) $ (.&.) op 0x0F00
          y = (`shiftR` 4) $ (.&.) op 0x00F0
          ks = keys emu
          rs = regs emu
          vx = rs!!x
          vy = rs!!y
          p = pc emu
          p' = ptr emu
          s = sp emu
          ss = stack emu
          m = mem emu
          dt = delay_timer emu
          st = sound_timer emu
