module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = do
  play window background fps world world_to_pic handle_events step_world

window :: Display
window = InWindow "Chip-8" (800, 600) (10, 10)

background :: Color
background = black

fps :: Int
fps = 1

world :: Int
world = 0

world_to_pic :: Int -> Picture
world_to_pic _ = Color white (circleSolid 5)

handle_events :: Event -> Int -> Int
handle_events _ w = w

step_world :: Float -> Int -> Int
step_world _ w = w
