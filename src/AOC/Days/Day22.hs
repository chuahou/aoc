-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day22 (solution) where

import           AOC.Parsec
import           AOC.Solution

import           Data.Set     (empty, insert, member)

----- SOLUTION -----

type Card = Int
type Game = ([Card], [Card])

data Winner = Player1 | Player2

-- in cases of ties we give player 1 the advantage, but this should never
-- happen due to unique cards and no cards leave the tuple
runGame :: Game -> [Card]
runGame ([], win) = win
runGame (win, []) = win
runGame (c1:cs1, c2:cs2)
    | c1 >= c2  = runGame (cs1 <> [c1, c2], cs2)
    | otherwise = runGame (cs1, cs2 <> [c2, c1])

runRecursiveGame :: Game -> [Card]
runRecursiveGame = snd . go empty
    where
        go _ ([], win) = (Player2, win)
        go _ (win, []) = (Player1, win)
        go prevs game@(c1:cs1, c2:cs2)
            | game `member` prevs = (Player1, c1:cs1)
            | length cs1 >= c1 && length cs2 >= c2 -- can recurse
                = case go prevs (take c1 cs1, take c2 cs2) of
                    (Player1, _) -> go prevs win1
                    (Player2, _) -> go prevs win2
            | c1 >= c2  = go (game `insert` prevs) win1
            | otherwise = go (game `insert` prevs) win2
            where
                win1 = (cs1 <> [c1, c2], cs2)
                win2 = (cs1, cs2 <> [c2, c1])

score :: [Card] -> Int
score = sum . zipWith (*) [1..] . reverse

----- PARSING -----

gameP :: Parser Game
gameP = (,) <$> (string "Player 1:\n" *> endBy1 cardP endOfLine <* endOfLine)
            <*> (string "Player 2:\n" *> endBy1 cardP endOfLine)
    where
        cardP = readP (many1 digit)

----- SKELETON -----

solution :: Game :=> Int
solution = simpleSolution
    (fromParsec gameP)
    (score . runGame)
    (score . runRecursiveGame)
