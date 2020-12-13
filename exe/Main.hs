-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module Main where

import           System.Environment (getArgs)
import           Text.Read          (readMaybe)

import           AOC                (runDay)

main :: IO ()
main = getArgs >>= \case
    [n] -> case readMaybe n of
             Just n' -> runDay n'
             Nothing -> error "Expected exactly 1 integer argument"
    _   -> error "Expected exactly 1 argument"
