module Main (main) where

import System.Random
import Data.ByteString as BS
import Debug.Trace
import Numeric

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Emulator
import Op

main :: IO ()
main = do
  rng <- getStdGen
  game <- BS.readFile "games/PONG"

  let emu = load_game (unpack game)
  let world = (rng, emu)
  play window background fps world draw_world handle_events step_world

window :: Display
window = InWindow "Chip-8" (800, 600) (10, 10)

background :: Color
background = black

fps :: Int
fps = 60

draw_world :: (RandomGen g) => World g -> Picture
draw_world _ | trace "Drawing world..." False = undefined
draw_world (_, emu) = render_emu emu

handle_events :: (RandomGen g) => Event -> World g -> World g
handle_events _ w = w

step_world :: (RandomGen g) => Float -> World g -> World g
-- step_world _ (_, emu) | trace ("Running instruction 0x" ++ (showHex (get_opcode emu) "...")) False = undefined
step_world _ (rng, emu) = case exec_op emu (get_opcode emu) rng of
  Left (rng, emu) -> (rng, decr_timers . incr_pc $ emu)
  Right err -> error err
    
