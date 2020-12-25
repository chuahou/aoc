-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou
--
-- I was unfamiliar with 'DataKinds', 'KnownNats' etc. so I have taken reference
-- from https://github.com/glguy/advent2020/blob/master/execs/Day25.hs.

{-# LANGUAGE DataKinds #-}

module AOC.Days.Day25 (solution) where

import           AOC.Parsec
import           AOC.Solution

import           Math.NumberTheory.Moduli
import           Math.NumberTheory.Moduli.Singleton

type P = 20201227

dh :: Int -> Int -> Int -> Maybe Int
dh g pkA pkB = do
    { cg <- cyclicGroup :: Maybe (CyclicGroup Integer P)
    ; rt <- isPrimitiveRoot cg (fromIntegral g :: Mod P)
    ; x  <- isMultElement (fromIntegral pkA :: Mod P)
    ; let skA = discreteLogarithm cg rt x
    ; pure . fromInteger . getVal $ powMod (fromIntegral pkB :: Mod P) skA
    }

solution :: (Int, Int) :=> Maybe Int
solution = simpleSolution
    (fromParsec $ (,) <$> readP (many1 digit) <* endOfLine
                      <*> readP (many1 digit))
    (uncurry $ dh g)
    (const Nothing) -- there's no part 2
    where
        g = 7        -- element
