-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day24 (solution) where

import           AOC.Parsec
import           AOC.Solution

import           Control.Monad      ((<=<))
import           Data.Bifunctor     (bimap)
import           Data.List          (sort)
import qualified Data.List.NonEmpty as NE
import           Data.Set           (Set)
import qualified Data.Set           as Set

{-
    O - O - O   : (1, 2) (2, 2) (3, 2)
   / \ / \ / \
  O - O - O - O : (0, 1) (1, 1) (2, 1) (3, 1)
   \ / \ / \ /
    O - O - O   : (0, 0) (1, 0) (2, 0)
    The x axis grows to the east and the y axis grows to the northwest.
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

initial :: [Instr] -> [(Int, Int)]
initial = map NE.head . filter (odd . length) . NE.group . sort

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = map (bimap (x+) (y+)) [ ( 1,  1)
                                          , ( 0,  1)
                                          , ( 0, -1)
                                          , (-1, -1)
                                          , ( 1,  0)
                                          , (-1,  0)
                                          ]

step :: Hex -> Hex
step h = (h `Set.union` gen) Set.\\ kill
    where
        allNeighbours = NE.group . sort . concat
                      . Set.toList . Set.map neighbours $ h
        gen   = (toSet . filter ((== 2) . length) $ allNeighbours) Set.\\ h
        kill  = (h Set.\\ toSet allNeighbours)
              `Set.union` (toSet . filter ((> 2) . length) $ allNeighbours)
        toSet = Set.fromList . map NE.head

solution :: Hex :=> Int
solution = simpleSolution
    (Just . Set.fromList . initial <=< fromParsec (many instrP))
    Set.size
    (Set.size . (!! 100) . iterate step)
