-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module AOC.Days.Day08 (solution) where

import           Control.Applicative ((<|>))
import           Data.Set            (Set, empty, insert, member)

import           AOC.Solution

data Instruction = Nop Int | Acc Int | Jmp Int | End deriving (Show, Eq)
type Program = [Instruction]
type Line = Int

parseProgram :: String -> Maybe Program
parseProgram = fmap (<> [End]) . mapM parseInstruction . lines

parseInstruction :: String -> Maybe Instruction
parseInstruction cs = let (instStr, _:sgn:numStr) = splitAt 3 cs
                          num = (if sgn == '+' then id else (-) 0) $ read numStr
                       in case instStr of
                            "acc" -> Just $ Acc num
                            "nop" -> Just $ Nop num
                            "jmp" -> Just $ Jmp num
                            _     -> Nothing

part1 :: Int -> Set Line -> Line -> Program -> Int
part1 acc visited line prog = if line `member` visited then acc else
    case prog !! line of
      Nop _ -> part1 acc       (insert line visited) (line + 1) prog
      Acc x -> part1 (acc + x) (insert line visited) (line + 1) prog
      Jmp x -> part1 acc       (insert line visited) (line + x) prog
      End   -> error "Did not loop"

part2 :: Int -> Set Line -> Line -> (Bool, Program) -> Maybe Int
part2 acc visited line (replaced, prog) =
    if line `member` visited then Nothing else
    case prog !! line of
      Acc x ->
        part2 (acc + x) (insert line visited) (line + 1) (replaced, prog)
      Nop x ->
        part2 acc (insert line visited) (line + 1) (replaced, prog)
            <|> replace (Jmp x)
      Jmp x ->
        part2 acc (insert line visited) (line + x) (replaced, prog)
            <|> replace (Nop x)
      End   -> Just acc
    where replace i = if replaced then Nothing else
                      part2 acc visited line (True, replace' i)
          replace' i = let (p1, _:p2) = splitAt line prog
                        in p1 <> (i:p2)

solution :: Program :=> Maybe Int
solution = simpleSolution
    parseProgram
    (Just . part1 0 empty 0)
    (part2 0 empty 0 . (,) False)
