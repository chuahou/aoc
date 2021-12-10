-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day04 (solution) where

import           AOC.Parsec
import           AOC.Solution
import           Data.List    (transpose)

type Board = [[Int]]
type Game = ([Int], [Board])

parser :: Parser Game
parser = do
    ns <- sepBy1 numP (char ',') <* string "\n\n"
    bs <- many1 boardP
    pure (ns, bs)

    where
        numP :: Parser Int
        numP = readP $ many1 digit

        boardP :: Parser Board
        boardP = count 5 $
            count 5 (many (char ' ') *> numP <* many (char ' '))
                <* many (char '\n')

-- All actual numbers >= 0, so we use -1 to indicate marked numbers. A
-- row/column will sum to -5 iff all are marked.
won :: Board -> Bool
won b = any (any ((== (-5)) . sum)) [ b, transpose b ]

step :: Game -> Maybe Game
step ([], _)    = Nothing
step (n:ns, bs) = Just (ns, map (map $ map $ \x -> if x == n then -1 else x) bs)

part1 :: Game -> Maybe Int
part1 ([], _) = Nothing
part1 g@(n:_, _) = do
    g'@(_, bs) <- step g
    case filter won bs of
      []    -> part1 g'
      (b:_) -> Just . (* n) . sum . filter (> 0) . concat $ b

solution :: Game :=> Maybe Int
solution = simpleSolution
    (fromParsec parser)
    part1
    undefined -- part2
