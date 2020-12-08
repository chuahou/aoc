-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import           Control.Monad (forM_)
import           Data.Set      (Set, empty, insert, member)

data Instruction = Nop Int | Acc Int | Jmp Int deriving (Show, Eq)
type Program = [Instruction]
type Line = Int

parseProgram :: String -> Program
parseProgram = map parseInstruction . lines

parseInstruction :: String -> Instruction
parseInstruction cs = let (instStr, _:sgn:numStr) = splitAt 3 cs
                          num = (if sgn == '+' then id else (-) 0) $ read numStr
                       in case instStr of
                            "acc" -> Acc num
                            "nop" -> Nop num
                            "jmp" -> Jmp num
                            _     -> error "Unknown instruction"

part1 :: Program -> Int
part1 = part1' 0 empty 0

part1' :: Int -> Set Line -> Line -> Program -> Int
part1' acc visited line prog = if line `member` visited then acc else
    case prog !! line of
      Nop _ -> part1' acc       (insert line visited) (line + 1) prog
      Acc x -> part1' (acc + x) (insert line visited) (line + 1) prog
      Jmp x -> part1' acc       (insert line visited) (line + x) prog

part2 = undefined

main :: IO ()
main = do { input <- parseProgram <$> readFile "input"
          ; forM_ [part1, part2] (\p -> print . p $ input)
          }
