module Emulator (
  Chip8,
  init_emu,
  render_emu
) where

import Graphics.Gloss

data Chip8 = Chip8 {
  mem            :: [Int],     -- 4096 1-byte address
  regs           :: [Int],     -- 16 registers
  stack          :: [Int],     -- 16(?) levels of addresses
  pc             :: Int,
  delay_timer    :: Int,
  sound_timer    :: Int,
  keyboard       :: [Bool],    -- 16 keys
  screen         :: [[Bool]]   -- 64x32 pixels
} deriving (Show)

fontset :: [Int]
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
  pc = 512,
  delay_timer = 0,
  sound_timer = 0,
  keyboard = take 16 $ repeat False,
  screen = take 64 . repeat . take 32 $ repeat False
}
  
square :: Int -> Int -> Picture
square r c = Polygon $ map f [(r,c), (r,c+1), (r+1,c+1), (r+1,c)]
  where f (r, c) = (800.0/32.0 * (fromIntegral c) - 400.0, 600.0/64.0 * (fromIntegral r) - 300.0)

render_emu :: Chip8 -> Picture
render_emu emu = pictures . map drawRow . zip [0..] $ screen emu
  where drawRow (r, row) = pictures . map (drawPixel r) $ zip [0..] row
        drawPixel r (c, pix)
          | pix       = Color white $ square r c
          | otherwise = Color black $ square r c