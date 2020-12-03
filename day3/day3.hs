-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import           Control.Monad (forM_)

data Cell = Empty | Tree deriving (Show, Eq)
type Map  = [[Cell]]

solve :: (Map -> a) -> String -> a
solve f = f . parse
    where
        parse    = map (cycle . map cell) . lines
        cell '.' = Empty
        cell '#' = Tree
        cell _   = error "Invalid map"

walk :: (Int, Int) -> Map -> Int
walk _     [c:_] = if c == Tree then 1 else 0
walk slope css   = (if c == Tree then 1 else 0) + (walk slope . step $ css)
    where
        c    = head . head $ css
        step = drop (snd slope) . map (drop (fst slope))

part1 :: Map -> Int
part1 = walk (3, 1)

main :: IO ()
main = do { input <- readFile "input"
          ; forM_ [part1] (\p -> print . solve p $ input)
          }
