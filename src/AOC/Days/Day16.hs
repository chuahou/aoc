-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day16 (solution) where

import           Data.List    (sortOn)

import           AOC.Parsec
import           AOC.Solution

----- SOLUTION -----

data Field = Field { name :: String, ranges :: [Range] } deriving Show
type Ticket = [Int]

invalidValues :: Ticket -> [Range] -> [Int]
invalidValues ts rs = filter (not . flip inRanges (mergeRanges rs)) ts

----- RANGE -----

-- Inclusive range of integers
type Range = (Int, Int)

-- Operations on ranges
mergeRanges :: [Range] -> [Range]
mergeRanges = mergeSorted . sortOn fst
    where
        mergeSorted []  = []
        mergeSorted [r] = [r]
        mergeSorted (r1@(a1,b1):r2@(a2,b2):rs)
            | b1 >= a2  = mergeSorted ((min a1 a2, max b1 b2):rs)
            | otherwise = r1 : mergeSorted (r2:rs)
inRange :: Int -> Range -> Bool
x `inRange` (a, b) = a <= x && x <= b
inRanges :: Int -> [Range] -> Bool
inRanges x = any (inRange x)

----- PARSING -----

data Input = Input [Field] Ticket [Ticket] deriving Show

inputP :: Parser Input
inputP = do { fs <- endBy fieldP endOfLine <* endOfLine
            ; t  <- string "your ticket:\n" *> ticketP <* many endOfLine
            ; ts <- string "nearby tickets:\n" *> endBy ticketP endOfLine
            ; return $ Input fs t ts
            }

fieldP :: Parser Field
fieldP = Field <$> (many1 (alphaNum <|> char ' ') <* string ": ")
               <*> (mergeRanges <$> sepBy rangeP (string " or "))

rangeP :: Parser Range
rangeP = (,) <$> readP (many1 digit) <*> (char '-' *> readP (many1 digit))

ticketP :: Parser Ticket
ticketP = sepBy1 (readP (many1 digit)) (char ',')

----- SKELETON ------

solution :: Input :=> Int
solution = simpleSolution
    (fromParsec inputP)
    (\(Input fs _ ts) ->
        sum . concatMap (`invalidValues` concatMap ranges fs) $ ts)
    undefined -- part2
