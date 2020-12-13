-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module Main where

import           System.Environment (getArgs)
import           Text.Read          (readMaybe)

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
                          >>= maybe (error "Parse error") putStrLn . f
             Nothing      ->  error "Invalid day number"
