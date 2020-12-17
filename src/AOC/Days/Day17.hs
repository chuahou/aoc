-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day17 (solution) where

import           Data.Set     (Set)
import qualified Data.Set     as Set

import           AOC.Solution

type Pos3 = (Int, Int, Int)
type Pos4 = (Int, Int, Int, Int)
type Grid = Set

neighbours3 :: Pos3 -> Set Pos3
neighbours3 (x, y, z) = Set.fromList
    [ (x', y', z')
    | x' <- [x-1 .. x+1]
    , y' <- [y-1 .. y+1]
    , z' <- [z-1 .. z+1]
    , x' /= x || y' /= y || z' /= z
    ]

neighbours4 :: Pos4 -> Set Pos4
neighbours4 (x, y, z, w) = Set.fromList
    [ (x', y', z', w')
    | x' <- [x-1 .. x+1]
    , y' <- [y-1 .. y+1]
    , z' <- [z-1 .. z+1]
    , w' <- [w-1 .. w+1]
    , x' /= x || y' /= y || z' /= z || w' /= w
    ]

activeNeighbours :: Ord a => (a -> Set a) -> a -> Grid a -> Int
activeNeighbours f p g = Set.size $ f p `Set.intersection` g

killSet :: Ord a => (a -> Set a) -> Grid a -> Set a
killSet f g = Set.filter (\p -> let n = activeNeighbours f p g
                                 in n /= 2 && n /= 3) g

genSet :: Ord a => (a -> Set a) -> Grid a -> Set a
genSet f g = let ns = Set.unions $ Set.map f g
              in Set.filter (\p -> activeNeighbours f p g == 3) ns

genKill :: Ord a => (a -> Set a) -> Grid a -> Grid a
genKill f g = (g `Set.union` genSet f g) Set.\\ killSet f g

expandDim :: Grid Pos3 -> Grid Pos4
expandDim = Set.map (\(x, y, z) -> (x, y, z, 0))

parse :: String -> Maybe (Grid Pos3)
parse = foldr lineP (Just Set.empty) . zip [0..] . lines
    where
        lineP (x, cs) g = foldr (charP x) g . zip [0..] $ cs
        charP _ (_, '.') g = g
        charP x (y, '#') g = Set.insert (x, y, 0) <$> g
        charP _ _        _ = Nothing

solution :: Grid Pos3 :=> Int
solution = simpleSolution
    parse
    (Set.size . (!! 6) . iterate (genKill neighbours3))
    (Set.size . (!! 6) . iterate (genKill neighbours4) . expandDim)
