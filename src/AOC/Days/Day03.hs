-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day03 (solution) where

import           AOC.Solution

data Cell = Empty | Tree deriving (Show, Eq)
type Map  = [[Cell]]

walk :: (Int, Int) -> Map -> Int
walk _     [c:_] = if c == Tree then 1 else 0
walk slope css   = (if c == Tree then 1 else 0) + (walk slope . step $ css)
    where
        c    = head . head $ css
        step = drop (snd slope) . map (drop (fst slope))

solution :: Map :=> Int
solution = simpleSolution
    (mapM (fmap cycle . mapM cell) . lines)
    (walk (3, 1))
    (\m -> product . map (`walk` m) $ [ (1, 1)
                                      , (3, 1)
                                      , (5, 1)
                                      , (7, 1)
                                      , (1, 2)
                                      ])
    where
        cell '.' = Just Empty
        cell '#' = Just Tree
        cell _   = Nothing
