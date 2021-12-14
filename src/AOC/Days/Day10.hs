-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day10 (solution) where

import           AOC.Solution
import           Data.Maybe   (mapMaybe)

illegal :: String -> Maybe Char
illegal = go []
    where
        go _        []       = Nothing
        go bs       ('(':cs) = go ('(':bs) cs
        go bs       ('[':cs) = go ('[':bs) cs
        go bs       ('{':cs) = go ('{':bs) cs
        go bs       ('<':cs) = go ('<':bs) cs
        go ('(':bs) (')':cs) = go bs cs
        go ('[':bs) (']':cs) = go bs cs
        go ('{':bs) ('}':cs) = go bs cs
        go ('<':bs) ('>':cs) = go bs cs
        go _        (c:_)    = Just c

badness :: Char -> Int
badness ')' = 3
badness ']' = 57
badness '}' = 1197
badness '>' = 25137
badness _   = 0

solution :: [String] :=> Int
solution = simpleSolution
    (pure . lines)
    (sum . map badness . mapMaybe illegal)
    undefined -- part2
