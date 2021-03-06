module Emulator (
  Chip8,
  mem,
  regs,
  stack,
  ptr,
  pc,
  sp,
  keys,
  screen,
  delay_timer,
  sound_timer,
  init_emu,
  render_emu,
  incr_pc,
  get_opcode,
  decr_timers,
  load_game,
  disp_state,
  beep,
) where

import qualified Data.Map as Map
import Data.Word
import Debug.Trace

import System.IO.Unsafe
import System.Process
-- TODO: Use os to make beep OS-agnostic
import System.Info

import Graphics.Gloss

data Chip8 = Chip8 {
  mem            :: [Word8],                  -- 4096 1-byte address
  regs           :: [Word8],                  -- 16 registers
  stack          :: [Int],                    -- 16(?) levels of addresses
  ptr            :: Int,                      -- I register (usually stores memory address)
  pc             :: Int,
  sp             :: Int,
  delay_timer    :: Int,
  sound_timer    :: Int,
  keys           :: [Bool],                   -- 16 keys
  screen         :: Map.Map (Int, Int) Bool   -- 64x32 pixels
} deriving (Show)

fontset :: [Word8]
fontset = [
  240, 144, 144, 144, 240,  -- 0
  32 , 96 , 32 , 32 , 112,  -- 1
  240, 16 , 240, 128, 240,  -- 2
  240, 16 , 240, 16 , 240,  -- 3
  144, 144, 240, 16 , 16 ,  -- 4
  240, 128, 240, 16 , 240,  -- 5
  240, 128, 240, 144, 240,  -- 6
  240, 16 , 32 , 64 , 64 ,  -- 7
  240, 144, 240, 144, 240,  -- 8
  240, 144, 240, 16 , 240,  -- 9
  240, 144, 240, 144, 144,  -- A
  224, 144, 224, 144, 224,  -- B
  240, 128, 128, 128, 240,  -- C
  224, 144, 144, 144, 224,  -- D
  240, 128, 240, 128, 240,  -- E
  240, 128, 240, 128, 128]  -- F


init_emu :: Chip8
init_emu = Chip8 {
  mem = fontset ++ (take 4016 $ repeat 0),
  regs = take 16 $ repeat 0,
  stack = take 16 $ repeat 0,
  ptr = 0,
  pc = 512,
  sp = 0,
  delay_timer = 0,
  sound_timer = 0,
  keys = take 16 $ repeat False,
  screen = Map.fromList [((r, c), False) | r <- [0..31], c <- [0..63]]
}
  
square :: Int -> Int -> Picture
square r c = Polygon $ map f [(r,c), (r,c+1), (r+1,c+1), (r+1,c)]
  where f (r, c) = (800.0/64.0 * (fromIntegral c) - 400.0, 600.0/32.0 * (fromIntegral r) - 300.0)

render_emu :: Chip8 -> Picture
render_emu emu = pictures . Map.foldrWithKey draw_pixel [] $ screen emu
  where draw_pixel (r, c) v lst = if v then (Color white $ square (31-r) c):lst else lst

-- opcodes are 2 bytes long
incr_pc :: Chip8 -> Chip8
incr_pc emu = emu{pc=p+2} where p = pc emu

get_opcode :: Chip8 -> Int
get_opcode emu = fromIntegral $ 256 * (toInteger (m!!p)) + (toInteger (m!!(p+1)))
  where m = mem emu
        p = pc emu

decr_timers :: Chip8 -> Chip8
decr_timers emu = emu{delay_timer = max 0 $ d-1, sound_timer = max 0 $ s-1}
  where d = delay_timer emu
        s = sound_timer emu

beep :: Chip8 -> Chip8
beep emu = if sound_timer emu /= 1 then emu else
             unsafePerformIO $ do
               createProcess $ shell "play beep.wav -q"
               return emu
             
load_game :: [Word8] -> Chip8
load_game game = init_emu{mem = take 4096 $ fontset ++ (take 432 (repeat 0)) ++ game ++ (repeat 0)}

disp_state :: Chip8 -> String
disp_state emu = "{\n" ++
  "\tregs = " ++ (show (regs emu)) ++ "\n" ++
  "\tptr  = " ++ (show (ptr emu)) ++ "\n" ++
  "\tsp   = " ++ (show (sp emu)) ++ "\n" ++
  "\tpc   = " ++ (show (pc emu)) ++ "\n" ++
  "}"
