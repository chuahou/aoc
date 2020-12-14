-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day06 (solution) where

import           AOC.Solution

group :: String -> [String]
group = group' ""

group' :: String -> String -> [String]
group' ys []             = [reverse ys]
group' ys ('\n':'\n':xs) = reverse ys : group' "" xs
group' ys (x:xs)         = group' (x:ys) xs

count :: [String] -> Int
count (cs:css) = length . foldr (filter . flip elem) cs $ css
count []       = 0

solution :: String :=> String
solution = Solution
    Just
    (const "Run script scripts/day06part1.sh directly")
    (show . sum . map (count . lines) . group)
    id
