-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day20 (solution) where

import           AOC.Parsec
import           AOC.Solution

----- SOLUTION -----

-- borders are E, S, W, N, with left-to-right in list = clockwise
data Tile   = Tile Int [[Cell]] deriving Show
data Cell   = Hash | Dot deriving Eq

instance Show Cell where
    show Hash = "#"
    show Dot  = "."

----- PARSING -----

cellP :: Parser Cell
cellP = (char '#' >> return Hash) <|> (char '.' >> return Dot)

tileP :: Parser Tile
tileP = do
    { num <- string "Tile " *> readP (many1 digit) <* char ':' <* endOfLine
    ; pic <- count 10 (count 10 cellP <* endOfLine)
    ; return $ Tile num pic
    }

----- SKELETON -----

solution :: [Tile] :=> Maybe Int
solution = simpleSolution
    (fromParsec (sepBy1 tileP endOfLine <* eof))
    undefined -- part1
    undefined -- part2
