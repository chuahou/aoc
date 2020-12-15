-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module Main where

import           Data.Maybe         (fromMaybe)
import           System.Environment (getArgs)

import           AOC                (getDay)
import           AOC.Days           (formatDay)

import           Paths_aoc

main :: IO ()
main = getArgs >>= \case
    [n] -> case readMaybe n of
             Just n' -> runDay n'
             Nothing -> error "Expected exactly 1 integer argument"
    _   -> error "Expected exactly 1 argument"

runDay :: Int -> IO ()
runDay n = case (,) <$> getDay n <*> formatDay n of
             Just (f, cs) ->  getDataFileName ("input/day" <> cs <> ".txt")
                          >>= readFile
                          >>= fromMaybe (error "Parse error") . f
             Nothing      ->  error "Invalid day number"
