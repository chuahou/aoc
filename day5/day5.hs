-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import           Control.Monad (forM_)

type Pass   = String
type Seat   = (Int, Int)
type SeatId = Int

solve :: ([Pass] -> a) -> String -> a
solve f = f . lines

part1 :: [Pass] -> Int
part1 = maximum . map (seatId . parse)

parse :: Pass -> Seat
parse ps = let (rowStr, colStr) = splitAt 7 ps
            in (toInt 'B' 'F' rowStr, toInt 'R' 'L' colStr)

toInt :: Char -> Char -> [Char] -> Int
toInt high low = foldl s 0
    where s n x | x == high = n * 2 + 1
                | x == low  = n * 2
                | otherwise = error "Parse error"

seatId :: Seat -> Int
seatId (r, c) = r * 8 + c

part2 :: [Pass] -> a
part2 = undefined

main :: IO ()
main = do { input <- readFile "input"
          ; forM_ [part1, part2] (\p -> print . solve p $ input)
          }
