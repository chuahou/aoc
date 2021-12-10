-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day07 (solution) where

import           AOC.Parsec
import           AOC.Solution

import           Data.List          (sort)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

type Pos = Int

freqs :: [Pos] -> [(Pos, Int)]
freqs = map (\(x:|xs) -> (x, NE.length $ x:|xs)) . NE.group . sort

cost :: [(Pos, Int)] -> Pos -> Int
cost fs p = sum $ map (\(p', n) -> abs (p - p') * n) fs

part1 :: [Pos] -> Maybe Pos
part1 ps = maximum ps >>= \maxPos -> minimum $ map (cost $ freqs ps) [0 .. maxPos]

solution :: [Pos] :=> Maybe Pos
solution = simpleSolution
    (fromParsec $ sepBy1 (readP $ many1 digit) (char ','))
    part1
    undefined -- part2
