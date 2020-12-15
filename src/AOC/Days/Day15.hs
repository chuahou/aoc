-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day15 (solution) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Map           (Map)
import qualified Data.Map           as Map

import           AOC.Parsec
import           AOC.Solution

(!?) :: [a] -> Int -> Maybe a
(x:_)  !? 0 = Just x
(_:xs) !? n = if n < 0 then Nothing else xs !? (n - 1)
[]     !? _ = Nothing

genNums :: NonEmpty Int -> [Int]
genNums input = genInput input Map.empty 0
    where
        genInput (x:|y:ys) m i = x : genInput (y:|ys) (Map.insert x i m) (i + 1)
        genInput (x:|[])   m i = x : 0 : genNums' (Map.insert x i m) (i + 1) 0

-- @genNums' m i prev@ generates everything after the @i@th element, where
-- @prev@ is the @i@th element and @m@ contains the last spoken of each number
-- before the @i@th element.
genNums' :: Map Int Int -> Int -> Int -> [Int]
genNums' m i prev = next : genNums' (Map.insert prev i m) (i + 1) next
    where
        next = maybe 0 (i -) $ Map.lookup prev m

solution :: NonEmpty Int :=> Maybe Int
solution = simpleSolution
    (fromParsec $ nonEmptyP $ sepBy1 (readP (many1 digit)) (char ','))
    ((!? 2019) . genNums)
    undefined -- part2
