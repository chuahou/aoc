-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day10 (solution) where

import           AOC.Solution
import           Data.List     (sort)
import           Data.Maybe    (mapMaybe)
import qualified Data.Sequence as Seq

-- Brackets should really be a fixed datatype, but lazy...

walk :: String -> Either String Char
walk = go []
    where
        go bs       []       = Left $ map flipBracket bs
        go bs       ('(':cs) = go ('(':bs) cs
        go bs       ('[':cs) = go ('[':bs) cs
        go bs       ('{':cs) = go ('{':bs) cs
        go bs       ('<':cs) = go ('<':bs) cs
        go ('(':bs) (')':cs) = go bs cs
        go ('[':bs) (']':cs) = go bs cs
        go ('{':bs) ('}':cs) = go bs cs
        go ('<':bs) ('>':cs) = go bs cs
        go _        (c:_)    = Right c

        flipBracket '(' = ')'
        flipBracket '[' = ']'
        flipBracket '{' = '}'
        flipBracket '<' = '>'
        flipBracket c   = c

badness :: Char -> Int
badness ')' = 3
badness ']' = 57
badness '}' = 1197
badness '>' = 25137
badness _   = 0

completeness :: String -> Int
completeness = go 0
    where
        go y []     = y
        go y (c:cs) = go (y * 5 + value c) cs
        value ')' = 1
        value ']' = 2
        value '}' = 3
        value '>' = 4
        value _   = 0

median :: Ord a => [a] -> a
median xs = Seq.index ys (Seq.length ys `div` 2)
    where ys = Seq.fromList $ sort xs

solution :: [String] :=> Int
solution = simpleSolution
    (pure . lines)
    (sum . map badness . mapMaybe (either (const Nothing) Just . walk))
    (median . map completeness . mapMaybe (either Just (const Nothing) . walk))
