-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day24 (solution) where

import           AOC.Parsec
import           AOC.Solution

import           Data.Bifunctor (bimap)
import           Data.Set       (Set)
import qualified Data.Set       as Set

{-
    O - O - O   : (1, 2) (2, 2) (3, 2)
   / \ / \ / \
  O - O - O - O : (0, 1) (1, 1) (2, 1) (3, 1)
   \ / \ / \ /
    O - O - O   : (0, 0) (1, 0) (2, 0)
    The x axis grows to the east and the y axis grows to the northeast.
-}
type Hex   = Set (Int, Int)
type Instr = (Int, Int)

instrP :: Parser Instr
instrP =   bimap sum sum . unzip <$> many go <* endOfLine
    where
        go =   (char 'n' >> choice [ char 'e' >> pure (1, 1)
                                   , char 'w' >> pure (0, 1)
                                   ])
           <|> (char 's' >> choice [ char 'e' >> pure ( 0, -1)
                                   , char 'w' >> pure (-1, -1)
                                   ])
           <|> (char 'e' >> pure ( 1, 0))
           <|> (char 'w' >> pure (-1, 0))

part1 :: [Instr] -> Hex
part1 = foldl' s Set.empty
    where
        s h i
            | i `Set.member` h = i `Set.delete` h
            | otherwise        = i `Set.insert` h

solution :: [Instr] :=> Int
solution = simpleSolution
    (fromParsec $ many instrP)
    (Set.size . part1)
    undefined -- part2
