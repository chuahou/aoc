-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

{-# OPTIONS_GHC -Wall #-}

import           Control.Monad (forM_)
import           Data.Maybe    (fromMaybe, mapMaybe)
import           Data.Vector   (Vector, (!?))
import qualified Data.Vector   as V

type ParsedInput = Vector (Vector Cell)
type Output      = Int

data Cell = Floor | Empty | Occupied deriving Eq

type Grid a = Vector (Vector a)

instance Show Cell where
    show Floor    = "."
    show Empty    = "L"
    show Occupied = "#"

(<!!>) :: Grid a -> (Int, Int) -> Maybe a
g <!!> (i, j) = g !? i >>= (!? j)

fromList :: [[a]] -> Grid a
fromList []       = V.fromList []
fromList (xs:xss) = let w = minimum . map length $ xs:xss
                     in V.fromList . map (V.fromList . take w) $ (xs:xss)

neighbours :: Grid a -> (Int, Int) -> [a]
neighbours g (i, j) = mapMaybe (g <!!>)
        [ (x, y) | x <- [i-1..i+1], y <-[j-1..j+1], x /= i || y /= j ]

rays :: Grid Cell -> (Int, Int) -> [Cell]
rays g ij = mapMaybe (cast ij) directions
    where
        directions = [ (x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0 ]
        cast (i0, j0) (di, dj) = foldr c Nothing $
            [ (i0 + n * di, j0 + n * dj) | n <- [1..] ]
        c ij' m = case g <!!> ij' of
                   Just Empty    -> Nothing
                   Just Occupied -> Just Occupied
                   Just Floor    -> m
                   Nothing       -> Nothing

indices :: Grid a -> [[(Int, Int)]]
indices xss = [ [ (x, y) | y <- [0..(V.length (V.head xss) - 1)] ]
                         | x <- [0..(V.length xss - 1)] ]

countP :: (a -> Bool) -> Grid a -> Int
countP p = V.length . V.concatMap (V.filter p)

step :: (Grid Cell -> (Int, Int) -> Int) -> Int -> Grid Cell -> Grid Cell
step count threshold g = fromList . map (map stepCell) $ indices g
    where stepCell (i, j) =
            case fromMaybe (error "Indexing error") $ g <!!> (i, j) of
              Floor    -> Floor
              Empty    -> if count g (i, j) > 0
                          then Empty else Occupied
              Occupied -> if count g (i, j) >= threshold
                          then Empty else Occupied

fix :: (Grid Cell -> Grid Cell) -> Grid Cell -> Grid Cell
fix f g = let g' = f g in if g == g' then g' else fix f g'

parse :: String -> Maybe ParsedInput
parse = fmap fromList . mapM (mapM cell) . lines
    where cell 'L' = Just Empty
          cell '.' = Just Floor
          cell _   = Nothing

part1 :: ParsedInput -> Output
part1 = countP (== Occupied) . fix (step count 4)
    where count g = length . filter (== Occupied) . neighbours g

part2 :: ParsedInput -> Output
part2 = countP (== Occupied) . fix (step count 5)
    where count g = length . filter (== Occupied) . rays g

main :: IO ()
main = do { input <- fromMaybe (error "Parse error") . parse
                      <$> readFile "input"
          ; forM_ [part1, part2] (\p -> print . p $ input)
          }
