-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day02 (solution) where

import           Data.Either  (fromRight)
import           Text.Parsec

import           AOC.Solution

type Requirement = (Int, Int, Char)
type Password    = String

solve :: (Requirement -> Password -> Bool) -> [String] -> Int
solve p = length . filter (fromRight False . parse (uncurry p <$> lineP) "")

lineP :: Parsec String () (Requirement, Password)
lineP = do { low  <- read <$> many1 digit <* char '-'
           ; high <- read <$> many1 digit <* space
           ; c    <- anyChar              <* string ": "
           ; cs   <- many anyChar         <* eof
           ; return ((low, high, c), cs)
           }

check1 :: Requirement -> Password -> Bool
check1 (low, high, c) cs =
    let cCount = length . filter (== c) $ cs
     in cCount <= high && cCount >= low

check2 :: Requirement -> Password -> Bool
check2 (x, y, c) cs =
    max x y <= length cs && (cs !! (x - 1) == c) /= (cs !! (y - 1) == c)

solution :: [String] :=> Int
solution = simpleSolution
    (Just . lines)
    (solve check1)
    (solve check2)
