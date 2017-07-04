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
  game <- BS.readFile "games/PONG2"

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
--draw_world _ | trace "Drawing world..." False = undefined
draw_world (_, emu) = render_emu emu

handle_events :: (RandomGen g) => Event -> World g -> World g
handle_events (EventKey (Char 'x') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x0 True })
handle_events (EventKey (Char 'x') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x0 False})
handle_events (EventKey (Char '1') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x1 True })
handle_events (EventKey (Char '1') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x1 False})
handle_events (EventKey (Char '2') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x2 True })
handle_events (EventKey (Char '2') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x2 False})
handle_events (EventKey (Char '3') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x3 True })
handle_events (EventKey (Char '3') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x3 False})
handle_events (EventKey (Char 'q') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x4 True })
handle_events (EventKey (Char 'q') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x4 False})
handle_events (EventKey (Char 'w') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x5 True })
handle_events (EventKey (Char 'w') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x5 False})
handle_events (EventKey (Char 'e') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x6 True })
handle_events (EventKey (Char 'e') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x6 False})
handle_events (EventKey (Char 'a') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x7 True })
handle_events (EventKey (Char 'a') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x7 False})
handle_events (EventKey (Char 's') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x8 True })
handle_events (EventKey (Char 's') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x8 False})
handle_events (EventKey (Char 'd') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x9 True })
handle_events (EventKey (Char 'd') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0x9 False})
handle_events (EventKey (Char 'z') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0xA True })
handle_events (EventKey (Char 'z') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0xA False})
handle_events (EventKey (Char 'c') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0xB True })
handle_events (EventKey (Char 'c') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0xB False})
handle_events (EventKey (Char '4') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0xC True })
handle_events (EventKey (Char '4') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0xC False})
handle_events (EventKey (Char 'r') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0xD True })
handle_events (EventKey (Char 'r') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0xD False})
handle_events (EventKey (Char 'f') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0xE True })
handle_events (EventKey (Char 'f') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0xE False})
handle_events (EventKey (Char 'v') Down _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0xF True })
handle_events (EventKey (Char 'v') Up   _ _) (rng, emu) = (rng, emu{keys=rpl_nth (keys emu) 0xF False})
handle_events _ w = w

step_world :: (RandomGen g) => Float -> World g -> World g
-- step_world _ (_, emu) | trace ("Running instruction 0x" ++ (showHex (get_opcode emu) "...")) False = undefined
step_world _ (rng, emu) = case exec_op emu (get_opcode emu) rng of
  Left (rng, emu) -> (rng, decr_timers . incr_pc $ emu)
  Right err -> error err
    
