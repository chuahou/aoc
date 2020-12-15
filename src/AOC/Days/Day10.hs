-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day10 (solution) where

import           Control.Monad      ((>=>))
import           Data.List          (sort)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import           AOC.Solution

type Jolts = Int

fromList :: [a] -> Maybe (NonEmpty a)
fromList (x:xs) = Just $ x :| xs
fromList []     = Nothing

part1 :: NonEmpty Jolts -> Jolts
part1 xs = let sorted = NE.sort xs
               diffs  = zipWith subtract (NE.toList sorted) (NE.tail sorted)
               diffs' = 3 : NE.head sorted : diffs
               ones   = length . filter (== 1) $ diffs'
               threes = length . filter (== 3) $ diffs'
            in ones * threes

part2 :: NonEmpty Jolts -> Jolts
part2 xs = let sorted = 0 : (sort . NE.toList $ xs)
            in ways sorted (length sorted - 1)

ways :: [Jolts] -> Int -> Int
ways xs = (table !!)
    where table = map f [0..]
          f 0 = 1
          f n = let feasible i = xs !! n - xs !! i <= 3
                 in sum . map (table !!) . filter feasible $ [0..n-1]

solution :: NonEmpty Jolts :=> Jolts
solution = simpleSolution
    (mapM readMaybe . lines >=> fromList)
    part1
    part2
