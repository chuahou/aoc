-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day16 (solution) where

import           Data.Bifunctor (first)
import           Data.Functor   ((<&>))
import           Data.List      (sortOn)

import           AOC.Parsec
import           AOC.Solution

----- SOLUTION -----

data Field = Field { name :: String, ranges :: [Range] } deriving Show
type Ticket = [Int]

invalidValues :: Ticket -> [Range] -> [Int]
invalidValues ts rs = filter (not . flip inRanges (mergeRanges rs)) ts

idFields :: [Ticket] -> [Field] -> Maybe [(Int, Field)]
idFields tss fs
    | all ((== length fs) . length) tss = assign . sortOn (length . fst)
                                        $ zip (map valid fs) fs
    | otherwise                         = Nothing -- length mismatch
    where
        -- remove invalid tickets
        tss' = filter (null . (`invalidValues` concatMap ranges fs)) tss

        -- check possible positions for each field
        valid :: Field -> [Int]
        valid f = filter (validPos f) [0..length fs - 1]
        validPos f pos = case mapM (!? pos) tss' of
                           Just xs -> all (`inRanges` ranges f) xs
                           Nothing -> False

        -- assign field positions based on possible positions
        assign :: [([Int], Field)] -> Maybe [(Int, Field)]
        assign []         = Just [] -- assigned all
        assign (([],_):_) = Nothing -- ran out of possibilities
        assign ((x:xs, f):ys) =   (((x, f):) <$> assign ys')
                              <|> assign ((xs, f):ys)
            where
                ys' = map (first $ filter (/= x)) ys -- remove choice from rest

getDepartureValues :: Ticket -> [(Int, Field)] -> Maybe [Int]
getDepartureValues t fs = mapM ((t !?) . fst)
                        $ filter ((== "departure") . take 9 . name . snd) fs

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

solution :: Input :=> Maybe Int
solution = simpleSolution
    (fromParsec inputP)
    (\(Input fs _ ts) ->
        Just . sum . concatMap (`invalidValues` concatMap ranges fs) $ ts)
    (\(Input fs t ts) ->
        idFields ts fs >>= getDepartureValues t <&> product)
