-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day09 (solution) where

import           Text.Read    (readMaybe)

import           AOC.Solution

type Window = [Int]

validate :: Window -> Int -> Bool
validate []     _ = False
validate (w:ws) x = (x - w) `elem` ws || validate ws x

findSet :: Int -> Int -> [Int] -> [Int] -> Maybe [Int]
findSet _ _   _   [] = Nothing
findSet y sum' set (x:xs)
    | sum' == y = if   length set >= 2
                  then Just set
                  else findSet y (sum' - head set + x) (tail set ++ [x]) xs
    | sum' <  y = findSet y (sum' + x) (set ++ [x]) xs
    | otherwise = findSet y (sum' - head set) (tail set) (x:xs)

part1 :: [Int] -> Maybe Int
part1 = uncurry part1' . splitAt 25
    where part1' (w:ws) (x:xs) = if   validate (w:ws) x
                                 then part1' (ws ++ [x]) xs
                                 else Just x
          part1' _      _      = Nothing

part2 :: [Int] -> Maybe Int
part2 xs = do { inval <- part1 xs
              ; set   <- findSet inval 0 [] xs
              ; return $ minimum set + maximum set
              }

solution :: [Int] :=> Maybe Int
solution = simpleSolution
    (mapM readMaybe . lines)
    part1
    part2
