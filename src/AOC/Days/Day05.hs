-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day05 (solution) where

import           Data.List    (sort)
import           Data.Maybe   (fromMaybe)

import           AOC.Solution

type Pass   = String
type SeatId = Int

parse :: Pass -> Maybe SeatId
parse ps = let (rowStr, colStr) = splitAt 7 ps
               (row, col) = (toInt 'B' 'F' rowStr, toInt 'R' 'L' colStr)
            in (,) <$> row <*> col >>= \(row', col') -> return $ row' * 8 + col'
            -- row * 8 + col

toInt :: Char -> Char -> [Char] -> Maybe Int
toInt high low = foldl (flip s) (Just 0)
    where s x | x == high = fmap $ (+1) . (*2)
              | x == low  = fmap (*2)
              | otherwise = const Nothing

solution :: [SeatId] :=> SeatId
solution = simpleSolution
    (mapM parse . lines)
    maximum
    (let find (x:y:ys) = if x + 2 == y then Just (x + 1) else find (y:ys)
         find _        = Nothing
      in fromMaybe (error "Could not find seat") . find . sort)
