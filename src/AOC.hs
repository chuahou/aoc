-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

{-# LANGUAGE TemplateHaskell, CPP #-}

module AOC (getDay) where

import           Data.Array     (bounds, inRange, (!))

import           AOC.Days
import qualified AOC.Days.Day01 as Day01
import qualified AOC.Days.Day02 as Day02
import qualified AOC.Days.Day03 as Day03
import qualified AOC.Days.Day04 as Day04
import qualified AOC.Days.Day05 as Day05
import qualified AOC.Days.Day06 as Day06
import qualified AOC.Days.Day07 as Day07
import qualified AOC.Days.Day08 as Day08
import qualified AOC.Days.Day09 as Day09
import qualified AOC.Days.Day10 as Day10
import qualified AOC.Days.Day11 as Day11
import qualified AOC.Days.Day12 as Day12
import qualified AOC.Days.Day13 as Day13
import qualified AOC.Days.Day14 as Day14
import qualified AOC.Days.Day15 as Day15
import qualified AOC.Days.Day16 as Day16
import qualified AOC.Days.Day17 as Day17
import qualified AOC.Days.Day18 as Day18
import qualified AOC.Days.Day19 as Day19
import qualified AOC.Days.Day20 as Day20
import qualified AOC.Days.Day21 as Day21
import qualified AOC.Days.Day22 as Day22
import qualified AOC.Days.Day23 as Day23
import qualified AOC.Days.Day24 as Day24
import qualified AOC.Days.Day25 as Day25

getDay :: Int -> Maybe (String -> Maybe (IO ()))
getDay n
    | inRange (bounds solns) n = Just $ solns ! n
    | otherwise                = Nothing
    where
        solns = -- TH causes nix's HLS to segfault for some reason, so this
                -- brings the TH out of scope for ghcide
#ifdef __GHCIDE__
            undefined
#else
            $(daySolutions)
#endif
