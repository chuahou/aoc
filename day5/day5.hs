-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import           Control.Monad (forM_)
import           Data.List     (sort)
import           Data.Maybe    (fromMaybe)

type Pass   = String
type Seat   = (Int, Int)
type SeatId = Int

parse :: Pass -> Seat
parse ps = let (rowStr, colStr) = splitAt 7 ps
            in (toInt 'B' 'F' rowStr, toInt 'R' 'L' colStr)

toInt :: Char -> Char -> [Char] -> Int
toInt high low = foldl s 0
    where s n x | x == high = n * 2 + 1
                | x == low  = n * 2
                | otherwise = error "Parse error"

seatId :: Seat -> SeatId
seatId (r, c) = r * 8 + c

solve :: ([SeatId] -> a) -> String -> a
solve f = f . map (seatId . parse) . lines

part1 :: [SeatId] -> SeatId
part1 = maximum

part2 :: [SeatId] -> SeatId
part2 xs = let ids      = sort xs
               find []  = Nothing
               find [_] = Nothing
               find (x:y:ys) = if x + 2 == y then Just (x + 1) else find (y:ys)
            in fromMaybe (error "Could not find seat") $ find ids

main :: IO ()
main = do { input <- readFile "input"
          ; forM_ [part1, part2] (\p -> print . solve p $ input)
          }
