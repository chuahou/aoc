-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

{-# LANGUAGE TupleSections #-}

import           Control.Applicative ((<|>))
import           Data.Functor        ((<&>))
import           Data.List           (sort)

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

target :: Int
target = 2020

solve1 :: [Int] -> Maybe Int
solve1 = (uncurry (*) <$>) . findPair target . sort

findPair :: Int -> [Int] -> Maybe (Int, Int)
findPair _ []     = Nothing
findPair t (x:xs) =   (x,) <$> (head' . filter (== t - x)) xs
                  <|> findPair t xs

solve2 :: [Int] -> Maybe Int
solve2 = fmap product' . findTriplet target . sort
    where product' (a, b, c) = product [a, b, c]

findTriplet :: Int -> [Int] -> Maybe (Int, Int, Int)
findTriplet _ []     = Nothing
findTriplet t (x:xs) =   (\(a, b) -> (x, a, b)) <$> findPair (t - x) xs
                     <|> findTriplet t xs

main :: IO ()
main = do { input <- readFile "input" <&> map read . lines
          ; print . solve1 $ input
          ; print . solve2 $ input
          }
