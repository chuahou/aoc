-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day01 (solution) where

import           AOC.Solution

part1 :: [Int] -> Int
part1 []     = 0
part1 (x:xs) = length . filter (uncurry (<)) $ zip (x:xs) xs

part2 :: [Int] -> Int
part2 (x:y:z:zs) = length . filter (uncurry (<)) $ zip (x:y:z:zs) zs
part2 _          = 0

solution :: [Int] :=> Int
solution = simpleSolution
    (traverse readMaybe . lines)
    part1
    part2
