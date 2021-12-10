-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day05 (solution) where

import           AOC.Parsec
import           AOC.Solution
import           Data.Array   (Array, (!), (//))
import qualified Data.Array   as A
import           Data.Maybe   (fromMaybe)

type Coord = (Int, Int)
type Vent = (Coord, Coord)
type Field = Array Coord Int

parser :: Parser [Vent]
parser = endBy1 ventP (char '\n')
    where
        ventP :: Parser Vent
        ventP = (,) <$> coordP <*> (string " -> " *> coordP)
        coordP :: Parser Coord
        coordP = (,) <$> numP <*> (char ',' *> numP)
        numP :: Parser Int
        numP = readP $ many1 digit

initField :: [Vent] -> Field
initField vs = A.listArray ((0, 0), size) zeroes
    where
        zeroes = 0 : zeroes
        size   = fromMaybe (0, 0) . go $ map fst vs ++ map snd vs
        go cs  = (,) <$> maximum (map fst cs) <*> maximum (map snd cs)

addVent :: Field -> Vent -> Field
addVent f ((x1, y1), (x2, y2))
    | x1 == x2  = incr [ (x1, y) | y <- [min y1 y2 .. max y1 y2] ]
    | y1 == y2  = incr [ (x, y1) | x <- [min x1 x2 .. max x1 x2] ]
    | otherwise = f
    where
        incr cs = f // [ (c, f ! c + 1) | c <- cs ]

part1 :: [Vent] -> Int
part1 vs = length . filter (> 1) . A.elems $ foldl' addVent (initField vs) vs

solution :: [Vent] :=> Int
solution = simpleSolution
    (fromParsec parser)
    part1
    undefined -- part2
