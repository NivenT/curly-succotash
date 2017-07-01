module Op (
  exec_op
) where 

import Emulator

exec_op :: Chip8 -> Int -> Int -> Int -> Int -> Either Chip8 String
exec_op emu op x y n
  | otherwise = Right $ "Instruction (" ++ (show op) ++ ") has not yet been implemented"
