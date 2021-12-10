-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day06 (solution) where

import           AOC.Parsec
import           AOC.Solution
import           Data.List          (sort)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe         (fromMaybe)

type Counts = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

parser :: Parser Counts
parser = do
    fs <- sepBy1 (readP $ many1 digit) (char ',') :: Parser [Int]
    let freqs = map (\(x:|xs) -> (x, NE.length $ x:|xs)) . NE.group . sort $ fs
    let lookup' n = fromMaybe 0 $ lookup n freqs
    pure (lookup' 0, lookup' 1, lookup' 2, lookup' 3, lookup' 4, lookup' 5,
            lookup' 6, lookup' 7, lookup' 8)

step :: Counts -> Counts
step (n0, n1, n2, n3, n4, n5, n6, n7, n8) =
    (n1, n2, n3, n4, n5, n6, n7 + n0, n8, n0)

sumCounts :: Counts -> Int
sumCounts (n0, n1, n2, n3, n4, n5, n6, n7, n8) =
    sum [n0, n1, n2, n3, n4, n5, n6, n7, n8]

solution :: Counts :=> Int
solution = simpleSolution
    (fromParsec parser)
    (sumCounts . (!! 80) . iterate step)
    undefined -- part2
