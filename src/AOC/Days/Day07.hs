-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day07 (solution) where

import           AOC.Parsec
import           AOC.Solution

import           Data.Array         (listArray, (!))
import           Data.List          (sort)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

type Pos = Int

freqs :: [Pos] -> [(Pos, Int)]
freqs = map (\(x:|xs) -> (x, NE.length $ x:|xs)) . NE.group . sort

cost1 :: Pos -> [(Pos, Int)] -> Pos -> Int
cost1 _ fs p = sum $ map (\(p', n) -> abs (p - p') * n) fs

cost2 :: Pos -> [(Pos, Int)] -> Pos -> Int
cost2 maxPos fs p = sum $ map (\(p', n) -> memo ! abs (p - p') * n) fs
    where
        memo = listArray (0, maxPos) (map triangle [0 .. maxPos])
        triangle 0 = 0
        triangle n = memo ! (n - 1) + n

solve :: (Pos -> [(Pos, Int)] -> Pos -> Int) -> [Pos] -> Maybe Pos
solve cost ps = maximum ps >>= \maxPos -> minimum $ map (cost maxPos $ freqs ps) [0 .. maxPos]

solution :: [Pos] :=> Maybe Pos
solution = simpleSolution
    (fromParsec $ sepBy1 (readP $ many1 digit) (char ','))
    (solve cost1)
    (solve cost2)
