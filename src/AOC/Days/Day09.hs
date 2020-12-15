-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day09 (solution) where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import           AOC.Solution

type Window = [Int]

validate :: Window -> Int -> Bool
validate []     _ = False
validate (w:ws) x = (x - w) `elem` ws || validate ws x

findSet :: Int -> Int -> NonEmpty Int -> [Int] -> Maybe [Int]
findSet _ _   _   [] = Nothing
findSet y sum' set (x:xs)
    | sum' == y = if   length set >= 2
                  then Just $ NE.toList set
                  else findSet y (sum' - NE.head set + x)
                                 (NE.tail set <+|+> x)
                                 xs
    | sum' <  y = findSet y (sum' + x) (set <++> x) xs
    | otherwise = case NE.tail set of
                    (t:ts) -> findSet y (sum' - NE.head set) (t:|ts) (x:xs)
                    []     -> findSet y (sum' + x)           (x:|[]) xs
    where
        appendNE  xs' x' = NE.reverse $ x' :| reverse xs'
        appendNE' ne  x' = appendNE (NE.toList ne) x'
        (<+|+>) = appendNE
        (<++>)  = appendNE'

part1 :: [Int] -> Maybe Int
part1 = uncurry part1' . splitAt 25
    where part1' (w:ws) (x:xs) = if   validate (w:ws) x
                                 then part1' (ws ++ [x]) xs
                                 else Just x
          part1' _      _      = Nothing

part2 :: [Int] -> Maybe Int
part2 xs = do { inval  <- part1 xs
              ; x      <- head xs
              ; xs'    <- tail xs
              ; set    <- findSet inval 0 (x:|[]) xs'
              ; minSet <- minimum set
              ; maxSet <- maximum set
              ; return $ minSet + maxSet
              }

solution :: [Int] :=> Maybe Int
solution = simpleSolution
    (mapM readMaybe . lines)
    part1
    part2
