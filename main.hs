module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Emulator
import Op

main :: IO ()
main = do
  play window background fps init_emu render_emu handle_events step_world

window :: Display
window = InWindow "Chip-8" (800, 600) (10, 10)

background :: Color
background = black

fps :: Int
fps = 60

handle_events :: Event -> Chip8 -> Chip8
handle_events _ w = w

step_world :: Float -> Chip8 -> Chip8
step_world _ w = case exec_op w (get_opcode w) of
  Left emu -> emu
  Right err -> error err
    
