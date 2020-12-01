-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

{-# LANGUAGE TupleSections #-}

import           Control.Applicative ((<|>))
import           Data.List           (sort)

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

target :: Int
target = 2020

solve :: [Int] -> Maybe Int
solve = (uncurry (*) <$>) . findPair . sort
    where findPair :: [Int] -> Maybe (Int, Int)
          findPair []     = Nothing
          findPair (x:xs) =   ((x,) <$> (head' . filter (== target - x)) xs)
                          <|> findPair xs

main :: IO ()
main =   readFile "input"
     >>= print . solve . map read . lines
