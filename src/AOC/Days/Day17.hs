-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day17 (solution) where

import           Data.Set     (Set)
import qualified Data.Set     as Set

import           AOC.Solution

type Pos  = (Int, Int, Int)
type Grid = Set Pos

neighbours :: Pos -> Set Pos
neighbours (x, y, z) = Set.fromList [ (x', y', z')
                                    | x' <- [x-1 .. x+1]
                                    , y' <- [y-1 .. y+1]
                                    , z' <- [z-1 .. z+1]
                                    , x' /= x || y' /= y || z' /= z
                                    ]

activeNeighbours :: Pos -> Grid -> Int
activeNeighbours p g = Set.size $ neighbours p `Set.intersection` g

killSet :: Grid -> Set Pos
killSet g = Set.filter (\p -> let n = activeNeighbours p g
                               in n /= 2 && n /= 3) g

genSet :: Grid -> Set Pos
genSet g = let ns = Set.unions $ Set.map neighbours g
            in Set.filter (\p -> activeNeighbours p g == 3) ns

genKill :: Grid -> Grid
genKill g = (g `Set.union` genSet g) Set.\\ killSet g

parse :: String -> Maybe Grid
parse = foldr lineP (Just Set.empty) . zip [0..] . lines
    where
        lineP (x, cs) g = foldr (charP x) g . zip [0..] $ cs
        charP _ (_, '.') g = g
        charP x (y, '#') g = Set.insert (x, y, 0) <$> g
        charP _ _        _ = Nothing

solution :: Grid :=> Int
solution = simpleSolution
    parse
    (Set.size . (!! 6) . iterate genKill)
    undefined -- part2
