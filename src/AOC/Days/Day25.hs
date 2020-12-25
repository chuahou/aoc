-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day25 (solution) where

import           AOC.Parsec
import           AOC.Solution

findSecret :: Int -> Int -> Int -> Int
findSecret g p pk = fst . (!! 0) . dropWhile ((/= pk) . snd) . zip [0..]
                  $ iterate ((`mod` p) . (* g)) 1

findSymmetric :: Int -> Int -> Int -> Int
findSymmetric p skA pkB = go skA pkB
    where
        go 1 x = x
        go n x = go (n - 1) $ x * pkB `mod` p

solution :: (Int, Int) :=> Int
solution = simpleSolution
    (fromParsec $ (,) <$> readP (many1 digit) <* endOfLine
                      <*> readP (many1 digit))
    (\(pkA, pkB) -> findSymmetric p (findSecret g p pkA) pkB)
    undefined
    where
        g = 7        -- element
        p = 20201227 -- prime
