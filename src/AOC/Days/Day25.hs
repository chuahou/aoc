-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day25 (solution) where

import           AOC.Parsec
import           AOC.Solution

import qualified Data.IntMap  as IntMap

dh :: Int -> Int -> Int -> Int -> Maybe Int
dh g p pkA pkB = powMod pkB <$> skA
    where
        skA = discreteLogarithm pkA

        opToMod :: (Integer -> Integer -> Integer) -> Int -> Int -> Int
        opToMod op x y = fromInteger
                       $ (toInteger x `op` toInteger y) `mod` toInteger p

        powMod :: Int -> Int -> Int
        (!x) `powMod` 1 = x
        (!x) `powMod` n = case n `divMod` 2 of
                            (n', 0) -> opToMod (^) x 2 `powMod` n'
                            (n', _) -> x `mulMod` (opToMod (^) x 2 `powMod` n')

        mulMod :: Int -> Int -> Int
        mulMod = opToMod (*)

        discreteLogarithm :: Int -> Maybe Int
        discreteLogarithm y = gMulInv >>= \inv -> Just (go inv y 0)
            where
                m    = floor ((sqrt . fromIntegral $ p) :: Float)
                memo = IntMap.fromList
                     . flip zip [0..] . take m $ iterate (mulMod g) 1
                gMulInv
                    | gcd g p == 1 = Just $ (g `powMod` (p - 2)) `powMod` m
                    | otherwise    = Nothing
                go inv y' i = case memo IntMap.!? y' of
                                Just j  -> i * m + j
                                Nothing -> go inv (y' `mulMod` inv) (i + 1)

solution :: (Int, Int) :=> Maybe Int
solution = simpleSolution
    (fromParsec $ (,) <$> readP (many1 digit) <* endOfLine
                      <*> readP (many1 digit))
    (uncurry $ dh g p)
    (const Nothing) -- there's no part 2
    where
        g = 7        -- element
        p = 20201227 -- prime
