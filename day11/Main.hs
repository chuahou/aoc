-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

{-# OPTIONS_GHC -Wall #-}

import           Control.Monad (forM_)
import           Data.Maybe    (fromMaybe, mapMaybe)

type ParsedInput = Grid Cell
type Output      = Int

data Cell = Floor | Empty | Occupied deriving Eq

data Grid a = Grid Int Int [[a]] deriving Eq

instance Show Cell where
    show Floor    = "."
    show Empty    = "L"
    show Occupied = "#"

instance Show a => Show (Grid a) where
    show (Grid _ _ css) = unlines . map (concatMap show) $ css

toGrid :: [[a]] -> Grid a
toGrid xss = let h = length xss
                 w = minimum . map length $ xss -- can't handle infinite!
              in Grid h w (map (take w) xss)

(<!!>) :: Grid a -> (Int, Int) -> Maybe a
(Grid h w css) <!!> (i, j)
    | i < h && i >= 0 && j < w && j >= 0 = Just $ css !! i !! j
    | otherwise                          = Nothing

neighbours :: Grid a -> (Int, Int) -> [a]
neighbours g (i, j) = mapMaybe (g <!!>) indices
    where
        indices = [ (x, y) | x <- [i-1..i+1], y <-[j-1..j+1], x /= i || y /= j ]

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

gridIndices :: Grid a -> [[(Int, Int)]]
gridIndices (Grid h w _) = [ [(x, y) | y <- [0..w-1] ] | x <- [0..h-1] ]

gridFilter :: (a -> Bool) -> Grid a -> [a]
gridFilter p (Grid _ _ css) = concatMap (filter p) css

step :: (Grid Cell -> (Int, Int) -> Int) -> Int -> Grid Cell -> Grid Cell
step count threshold g = toGrid . map (map stepCell) $ indices g
    where stepCell (i, j) =
            case fromMaybe (error "Indexing error") $ g <!!> (i, j) of
              Floor    -> Floor
              Empty    -> if count g (i, j) > 0
                          then Empty else Occupied
              Occupied -> if count g (i, j) >= threshold
                          then Empty else Occupied
          indices (Grid h w _) = [ [ (x, y) | y <- [0..w-1] ] | x <- [0..h-1] ]

fix :: (Grid Cell -> Grid Cell) -> Grid Cell -> Grid Cell
fix f g = let g' = f g in if g == g' then g' else fix f g'

parse :: String -> Maybe ParsedInput
parse = fmap toGrid . mapM (mapM cell) . lines
    where cell 'L' = Just Empty
          cell '.' = Just Floor
          cell _   = Nothing

part1 :: ParsedInput -> Output
part1 = length . gridFilter (== Occupied) . fix (step count 4)
    where count g = length . filter (== Occupied) . neighbours g

part2 :: ParsedInput -> Output
part2 = length . gridFilter (== Occupied) . fix (step count 5)
    where count g = length . filter (== Occupied) . rays g

main :: IO ()
main = do { input <- fromMaybe (error "Parse error") . parse
                      <$> readFile "input"
          ; forM_ [part1, part2] (\p -> print . p $ input)
          }
