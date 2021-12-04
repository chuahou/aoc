-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day03 (solution) where

import           AOC.Parsec
import           AOC.Solution
import           Data.List    (transpose)

data Bit = Zero | One deriving (Show, Eq)
type Number = [Bit]

bitP :: Parser Bit
bitP = (char '1' >> pure One) <|> (char '0' >> pure Zero)

part1 :: [Number] -> Int
part1 xs =
    let numOnes = map (length . filter (== One)) . transpose $ xs
        numNums = length xs
        gamma   = map (\n -> if n > numNums `div` 2 then One else Zero) numOnes
        epsilon = map (\case One -> Zero; Zero -> One) gamma
     in fromNumber gamma * fromNumber epsilon

fromNumber :: Num a => Number -> a
fromNumber = foldl' s 0
    where
        s n One  = s n Zero + 1
        s n Zero = n * 2

solution :: [Number] :=> Int
solution = simpleSolution
    (fromParsec $ endBy (many bitP) (char '\n'))
    part1
    undefined -- part2
