-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day01 (solution) where

import           AOC.Solution
import           Control.Monad ((>=>))

go :: Int -> [Int] -> Int
go n xs = maybe 0 (length . filter (uncurry (<)) . zip xs) $
    foldr (>=>) pure (replicate n tail) xs

solution :: [Int] :=> Int
solution = simpleSolution (traverse readMaybe . lines) (go 1) (go 3)
