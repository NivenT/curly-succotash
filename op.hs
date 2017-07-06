module Op (
  exec_op,
  World,
  rpl_nth
) where 

import System.Random
import Data.Bits
import Data.Word
import Data.List.Split
import qualified Data.Map as Map
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
draw_sprite emu x y h = emu{screen=s', regs=rpl_nth rs 0xf (if flag then 1 else 0)}
  where m = mem emu
        rs = regs emu
        vx = rs!!x
        vy = rs!!y
        p = ptr emu
        indices = map (\(r,c) -> (mod r 64, mod c 32)) . flatten $
          map (\r -> map (\c -> (fromIntegral vy+r, fromIntegral vx+c)) [0..7])  [0..h-1]
        s = Map.mapWithKey xor_pixel $ screen emu
        xor_pixel (r, c) v
          | (r, c) `elem` indices = let r' = fromIntegral $ fromIntegral r-vy
                                        c' = fromIntegral $ fromIntegral c-vx
                                        bit = 0 /= ((.&.) (m!!(p+r')) (shiftR 128 c'))
                                    in (xor v bit,  bit && (bit == v))
          | otherwise             = (v, False)
        s' = Map.map (\(v, b) -> v) s
        flag = Map.foldr (\(v, b) acc -> b || acc) False s

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

get_key :: [Bool] -> Maybe Int
get_key ks = foldl (\acc (ind, down) -> if down then Just ind else acc) Nothing . zip [0..] $ ks

-- Todo: Replace `elem` with in_range function
exec_op :: (RandomGen g) => Chip8 -> Int -> g -> Either (World g) String
exec_op emu op rng
  -- | trace ("Running instruction 0x" ++ (showHex op " ") {- ++ (disp_state emu) -}) False = undefined
  -- 0x00E0 clears the screen
  | op == 0x00e0               = Left (rng, emu{screen = screen init_emu})
  -- 0x00EE returns from a subroutine
  | op == 0x00ee               = Left (rng, emu{pc=ss!!(s-1), sp=s-1})
  -- 0x1NNN goto NNN;
  | in_range op 0x1000 0x1000  = let n = fromIntegral $ (.&.) op 0x0fff
                                 in Left (rng, emu{pc=n-2})
  -- 0x2NNN Call subroutine at NNN
  | in_range op 0x2000 0x1000  = let n = fromIntegral $ (.&.) op 0x0fff
                                 in Left (rng, emu{pc=n-2, sp=s+1, stack=rpl_nth ss s p})
  -- 0x3XNN skip if (VX == NN)
  | in_range op 0x3000 0x1000  = let n = fromIntegral $ (.&.) op 0x00ff
                                 in if n == vx
                                       then Left (rng, emu{pc=p+2})
                                       else Left (rng, emu)
  -- 0x4XNN skip if (VX != NN)
  | in_range op 0x4000 0x1000  = let n = fromIntegral $ (.&.) op 0x00ff
                                 in if n == vx
                                       then Left (rng, emu)
                                       else Left (rng, emu{pc=p+2})
  -- 0x5XY0 skip if (VX == VY)
  | in_range op 0x5000 0x1000  = if vx == vy
                                    then Left (rng, emu{pc=p+2})
                                    else Left (rng, emu)
  -- 0x6XNN VX = NN
  | in_range op 0x6000 0x1000  = let n = fromIntegral $ (.&.) op 0x00ff
                                 in Left (rng, emu{regs=rpl_nth rs x n})
  -- 0x7XNN VX += NN
  | in_range op 0x7000 0x1000  = let n = fromIntegral $ (.&.) op 0x00ff
                                 in Left (rng, emu{regs=rpl_nth rs x $ vx+n})
  -- 0x8XYA permform arithmetic operation A (see alu) on VX and VY 
  | in_range op 0x8000 0x1000  = Left(rng, alu emu x vx vy $ (.&.) op 0x000f)
  -- 0x9XY0 skip if (VX != VY)
  | in_range op 0x9000 0x1000  = if vx == vy
                                    then Left (rng, emu)
                                    else Left (rng, emu{pc=p+2})
  -- 0xANNN I = NNN (In the code, I is ptr because I is a stupid name)
  | in_range op 0xa000 0x1000  = let n = fromIntegral $ (.&.) op 0x0fff
                                 in Left (rng, emu{ptr=n})
  -- 0xBNNN PC=V0+NNN
  | in_range op 0xb000 0x1000  = let n = (.&.) op 0x0fff
                                     v = rs!!0
                                 in Left (rng, emu{pc=fromIntegral v+n-2})
  -- 0xCXNN VX=rand() & NN
  | in_range op 0xc000 0x1000  = let (r, g') = rand_byte rng
                                     n       = fromIntegral $ (.&.) op 0x00ff
                                     val     = (.&.) r n
                                 in Left (g', emu{regs=rpl_nth rs x val})
  -- 0xDXYN Draw the sprite in memory location I at screen location (VX, VY)
  | in_range op 0xd000 0x1000  = Left (rng, draw_sprite emu x y $ (.&.) op 0x000f)
  -- 0xEX9E skip if (keys[VX])
  | (.&.) op 0xf0ff == 0xe09e  = if ks!!(fromIntegral . toInteger $ vx)
                                    then Left (rng, emu{pc=p+2})
                                    else Left (rng, emu)
  -- 0xEXA1 skip if (!keys[VX])
  | (.&.) op 0xf0ff == 0xe0a1  = if ks!!(fromIntegral . toInteger $ vx)
                                    then Left (rng, emu)
                                    else Left (rng, emu{pc=p+2})
  -- 0xFX07 VX = delay_timer
  | (.&.) op 0xf0ff == 0xf007  = Left (rng, emu{regs=rpl_nth rs x $ fromIntegral dt})
  -- 0xFX0A Wait for a key press, then store key in VX (note: blocking operation)
  | (.&.) op 0xf0ff == 0xf00a  = case get_key ks of
                                   Just i  -> Left(rng, emu{regs=rpl_nth rs x $ fromIntegral i})
                                   Nothing -> Left(rng, emu{pc=p-2})
  -- 0xFX15 delay_timer = VX
  | (.&.) op 0xf0ff == 0xf015  = Left (rng, emu{delay_timer=fromIntegral vx})
  -- 0xFX18 sound_timer = VX
  | (.&.) op 0xf0ff == 0xf018  = Left (rng, emu{sound_timer=fromIntegral vx})
  -- 0xFX1E I += VX
  | (.&.) op 0xf0ff == 0xf01e  = Left (rng, emu{ptr=fromIntegral $ p' + fromIntegral vx})
  -- 0xFX29 Sets I to the location of the sprite for the character in VX
  | (.&.) op 0xf0ff == 0xf029  = Left (rng, emu{ptr=5 * fromIntegral vx})
  -- 0xFX33 Stores binary-coded decimal representation of VX at the address in I (MSD first)
  | (.&.) op 0xf0ff == 0xf033  = Left (rng, emu{mem=rpl m . zip [p'+2, p'+1, p'] $ digits vx})
  -- 0xFX55 Stores V0 to VX (inclusive) in memory starting at address I
  | (.&.) op 0xf0ff == 0xf055  = Left (rng, emu{mem=rpl m . zip [p'..p'+x] . take (x+1) $ rs})
  -- 0xFX65 Fills V0 to VX (inclusive) with values from memory starting at address I
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
