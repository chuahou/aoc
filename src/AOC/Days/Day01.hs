-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day01 (solution) where

import           Control.Applicative ((<|>))
import           Data.List           (sort)
import           Text.Read           (readMaybe)

import           AOC.Solution

solution :: [Int] :=> Maybe Int
solution = simpleSolution
    (mapM readMaybe . lines)
    ((uncurry (*) <$>) . findPair 2020 . sort)
    (fmap product' . findTriplet 2020 . sort)
    where
        product' (a, b, c) = a * b * c

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

findPair :: Int -> [Int] -> Maybe (Int, Int)
findPair _ []     = Nothing
findPair t (x:xs) =   (x,) <$> (head' . filter (== t - x)) xs
                  <|> findPair t xs

findTriplet :: Int -> [Int] -> Maybe (Int, Int, Int)
findTriplet _ []     = Nothing
findTriplet t (x:xs) =   (\(a, b) -> (x, a, b)) <$> findPair (t - x) xs
                     <|> findTriplet t xs
