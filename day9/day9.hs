-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import           Control.Monad (forM_)

type ParsedInput = [Int]
type Output      = Int

type Window = [Int]

validate :: Window -> Int -> Bool
validate []     _ = False
validate (w:ws) x = (x - w) `elem` ws || validate ws x

findSet :: Int -> [Int] -> [Int] -> [Int]
findSet _ _   [] = error "Could not find contiguous set"
findSet y set (x:xs) | sumSet == y = if   length set >= 2
                                     then set
                                     else findSet y (tail set ++ [x]) xs
                     | sumSet <  y = findSet y (set ++ [x]) xs
                     | otherwise   = findSet y (tail set)   (x:xs)
    where sumSet = sum set

parse :: String -> ParsedInput
parse = map read . lines

part1 :: ParsedInput -> Output
part1 = uncurry part1' . splitAt 25
    where part1' (w:ws) (x:xs) = if   validate (w:ws) x
                                 then part1' (ws ++ [x]) xs
                                 else x

part2 :: ParsedInput -> Output
part2 xs = let set = findSet (part1 xs) [] xs
            in minimum set + maximum set

main :: IO ()
main = do { input <- parse <$> readFile "input"
          ; forM_ [part1, part2] (\p -> print . p $ input)
          }
