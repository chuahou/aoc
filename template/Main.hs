-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

{-# OPTIONS_GHC -Wall #-}

import           Control.Monad (forM_)
import           Data.Maybe    (fromMaybe)

type ParsedInput = [Int]
type Output      = Int

parse :: String -> Maybe ParsedInput
parse = undefined

part1 :: ParsedInput -> Output
part1 = undefined

part2 :: ParsedInput -> Output
part2 = undefined

runFile :: String -> IO ()
runFile s = do { input <- fromMaybe (error "Parse error") . parse <$> readFile s
               ; forM_ [part1, part2] (\p -> print . p $ input)
               }

main :: IO ()
main = runFile "input"
