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
    (uncurry (*) . foldl' (\(x, y) (dx, dy) -> (x + dx, y + dy)) (0, 0))
    (uncurry (*) . fst . foldl' move ((0, 0), 0))
    where
        move ((x, y), aim) (dx, da) = ((x + dx, y + dx * aim), aim + da)
