-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day01 (solution) where

import           AOC.Solution

part1 :: [Int] -> Int
part1 []     = 0
part1 (x:xs) = length . filter (uncurry (<)) $ zip (x:xs) xs

solution :: [Int] :=> Int
solution = simpleSolution
    (traverse readMaybe . lines)
    part1
    undefined -- part2
