-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day02 (solution) where

import           AOC.Parsec
import           AOC.Solution
import           Data.Bifunctor (bimap)

type Instruction = (Int, Int)

instructionP :: Parser Instruction
instructionP = do
    dirn <- choice $ map try [ string "forward" >> pure (1,  0)
                             , string "down"    >> pure (0,  1)
                             , string "up"      >> pure (0, -1)
                             ]
    _    <- char ' '
    num  <- readP $ many digit
    pure $ bimap (* num) (* num) dirn

solution :: [Instruction] :=> Int
solution = simpleSolution
    (fromParsec $ sepEndBy instructionP (char '\n'))
    (uncurry (*) . foldl' (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) (0, 0))
    undefined -- part2
