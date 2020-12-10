-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import           Control.Applicative ((<|>))
import           Control.Monad       (forM_)
import           Data.Maybe          (fromMaybe)
import           Data.Set            (Set, empty, insert, member)

data Instruction = Nop Int | Acc Int | Jmp Int | End deriving (Show, Eq)
type Program = [Instruction]
type Line = Int

parseProgram :: String -> Program
parseProgram = (<> [End]) . map parseInstruction . lines

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
      End   -> error "Did not loop"

part2 :: Program -> Int
part2 = fromMaybe (error "Could not fix") . part2' 0 empty 0 . (,) False

part2' :: Int -> Set Line -> Line -> (Bool, Program) -> Maybe Int
part2' acc visited line (replaced, prog) =
    if line `member` visited then Nothing else
    case prog !! line of
      Acc x ->
        part2' (acc + x) (insert line visited) (line + 1) (replaced, prog)
      Nop x ->
        part2' acc (insert line visited) (line + 1) (replaced, prog)
            <|> replace (Jmp x)
      Jmp x ->
        part2' acc (insert line visited) (line + x) (replaced, prog)
            <|> replace (Nop x)
      End   -> Just acc
    where replace i = if replaced then Nothing else
                      part2' acc visited line (True, replace' i)
          replace' i = let (p1, _:p2) = splitAt line prog
                        in p1 <> (i:p2)

main :: IO ()
main = do { input <- parseProgram <$> readFile "input"
          ; forM_ [part1, part2] (\p -> print . p $ input)
          }
