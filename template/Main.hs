-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import           Control.Monad (forM_)

type ParsedInput = [Int]
type Output      = Int

parse :: String -> ParsedInput
parse = undefined

part1 :: ParsedInput -> Output
part1 = undefined

part2 :: ParsedInput -> Output
part2 = undefined

main :: IO ()
main = do { input <- parse <$> readFile "input"
          ; forM_ [part1, part2] (\p -> print . p $ input)
          }
