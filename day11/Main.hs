-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

{-# OPTIONS_GHC -Wall #-}

import           Control.Monad (forM_)
import           Data.Maybe    (fromMaybe, mapMaybe)

type ParsedInput = Grid Cell
type Output      = Int

data Cell = Floor | Empty | Occupied deriving Eq

data Grid a = Grid { height :: Int
                   , width  :: Int
                   , cells  :: [[a]]
                   } deriving Eq

instance Show Cell where
    show Floor    = "."
    show Empty    = "L"
    show Occupied = "#"

instance Show a => Show (Grid a) where
    show (Grid _ _ css) = unlines . map (concatMap show) $ css

gridFromList :: [[a]] -> Grid a
gridFromList xss = let h = length xss
                       w = minimum . map length $ xss -- can't handle infinite!
                    in Grid h w (map (take w) xss)

gridIndex :: Grid a -> Int -> Int -> Maybe a
gridIndex (Grid h w css) i j
    | i < h && i >= 0 && j < w && j >= 0 = Just $ css !! i !! j
    | otherwise                          = Nothing

(<!!>) :: Grid a -> (Int, Int) -> Maybe a
(<!!>) g = uncurry $ gridIndex g

gridNeighbours :: Grid a -> Int -> Int -> [a]
gridNeighbours g i j = mapMaybe (g <!!>) indices
    where
        indices = [ (x, y) | x <- [i-1..i+1], y <-[j-1..j+1], x /= i || y /= j ]

gridVisible :: Grid Cell -> Int -> Int -> [Cell]
gridVisible g i j = mapMaybe (moveDir (i, j)) directions
    where
        directions = [ (x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0 ]
            :: [(Int, Int)]
        moveDir (i', j') (di, dj) =
            let i'' = i' + di
                j'' = j' + dj
             in case g <!!> (i'', j'') of
                  Just Occupied -> Just Occupied
                  Just Empty    -> Nothing
                  Just Floor    -> moveDir (i'', j'') (di, dj)
                  Nothing       -> Nothing

gridIndices :: Grid a -> [[(Int, Int)]]
gridIndices (Grid h w _) = [ [(x, y) | y <- [0..w-1] ] | x <- [0..h-1] ]

gridFilter :: (a -> Bool) -> Grid a -> [a]
gridFilter p (Grid _ _ css) = concatMap (filter p) css

step :: Grid Cell -> Grid Cell
step g = gridFromList . map (map stepCell) . gridIndices $ g
    where
        stepCell (i, j) =
            case fromMaybe (error "Indexing error") $ g <!!> (i, j) of
              Floor    -> Floor
              Empty    -> if null $ occupiedNeighbours i j
                          then Occupied else Empty
              Occupied -> if length (occupiedNeighbours i j) >= 4
                          then Empty else Occupied
        occupiedNeighbours i j = filter (== Occupied) $ gridNeighbours g i j

step' :: Grid Cell -> Grid Cell
step' g = gridFromList . map (map stepCell) . gridIndices $ g
    where
        stepCell (i, j) =
            case fromMaybe (error "Indexing error") $ g <!!> (i, j) of
              Floor    -> Floor
              Empty    -> if null $ occupiedVisible i j
                          then Occupied else Empty
              Occupied -> if length (occupiedVisible i j) >= 5
                          then Empty else Occupied
        occupiedVisible i j = filter (== Occupied) $ gridVisible g i j

parse :: String -> Maybe ParsedInput
parse = fmap gridFromList . mapM (mapM cell) . lines
    where cell 'L' = Just Empty
          cell '.' = Just Floor
          cell _   = Nothing

part1 :: ParsedInput -> Output
part1 = length . gridFilter (== Occupied) . fix
    where fix g = let g' = step g
                   in if g == g' then g else fix g'

part2 :: ParsedInput -> Output
part2 = length . gridFilter (== Occupied) . fix
    where fix g = let g' = step' g
                   in if g == g' then g else fix g'

main :: IO ()
main = do { input <- fromMaybe (error "Parse error") . parse
                      <$> readFile "input"
          ; forM_ [part1, part2] (\p -> print . p $ input)
          }
