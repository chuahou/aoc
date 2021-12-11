-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day08 (solution) where

import           AOC.Parsec
import           AOC.Solution

data Segment = A | B | C | D | E | F | G deriving (Show, Eq)
type Pattern = [Segment]
type Entry   = ([Pattern], [Pattern])

entryP :: Parser Entry
entryP = do
    uniq <- count 10 (patternP <* char ' ') <* char '|'
    outs <- count 4  (char ' ' *> patternP)
    pure (uniq, outs)
    where
        patternP = many1 segmentP
        segmentP = choice [ char 'a' >> pure A
                          , char 'b' >> pure B
                          , char 'c' >> pure C
                          , char 'd' >> pure D
                          , char 'e' >> pure E
                          , char 'f' >> pure F
                          , char 'g' >> pure G
                          ]

solution :: [Entry] :=> Int
solution = simpleSolution
    (fromParsec $ endBy entryP (char '\n'))
    (length . filter ((`elem` [2, 3, 4, 7]) . length) . concatMap snd)
    undefined -- part2
