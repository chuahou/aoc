-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import           Control.Monad (forM_)
import           Data.List     (sort)
import           Data.Maybe    (fromMaybe)

type Pass   = String
type SeatId = Int

parse :: Pass -> SeatId
parse ps = let (rowStr, colStr) = splitAt 7 ps
               (row, col) = (toInt 'B' 'F' rowStr, toInt 'R' 'L' colStr)
            in row * 8 + col

toInt :: Char -> Char -> [Char] -> Int
toInt high low = foldl (flip s) 0
    where s x | x == high = (+1) . (*2)
              | x == low  = (*2)
              | otherwise = error "Parse error"

solve :: ([SeatId] -> a) -> String -> a
solve f = f . map parse . lines

part1 :: [SeatId] -> SeatId
part1 = maximum

part2 :: [SeatId] -> SeatId
part2 = let find (x:y:ys) = if x + 2 == y then Just (x + 1) else find (y:ys)
            find _        = Nothing
         in fromMaybe (error "Could not find seat") . find . sort

main :: IO ()
main = do { input <- readFile "input"
          ; forM_ [part1, part2] (\p -> print . solve p $ input)
          }
