-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day15 (solution) where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import           AOC.Parsec
import           AOC.Solution

genNums :: NonEmpty Int -> Int -> Maybe Int
genNums input target
    | target <= 0                   = Nothing
    | target <= NE.length input     = Just $ input NE.!! (target - 1)
    | target == NE.length input + 1 = Just 0
    | otherwise                     = Just $ genInput input Map.empty 1
    where
        genInput (x:|y:ys) m i = genInput (y:|ys) (Map.insert x i m) (i + 1)
        genInput (x:|[])   m i = genNums' (Map.insert x i m) (i + 1) 0 target

-- @genNums' m i prev@ calculates the @i+1@th element, where @prev@ is the @i@th
-- element and @m@ contains the last spoken of each number before the @i@th
-- element. It returns the previous element when @i == target@.
genNums' :: IntMap Int -> Int -> Int -> Int -> Int
genNums' !m !i !prev target
    | i == target = prev
    | otherwise   = genNums' (Map.insert prev i m) (i + 1) next target
    where
        next = maybe 0 (i -) $ Map.lookup prev m

solution :: NonEmpty Int :=> Maybe Int
solution = simpleSolution
    (fromParsec $ nonEmptyP $ sepBy1 (readP (many1 digit)) (char ','))
    (`genNums` 2020)
    (`genNums` 30000000)
