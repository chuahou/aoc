-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day09 (solution) where

import           Prelude         hiding ((!?))

import           AOC.Parsec
import           AOC.Solution
import           Data.Graph      (Graph, Vertex)
import qualified Data.Graph      as Graph
import           Data.List       (sort)
import           Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import           Data.Maybe      (isJust, mapMaybe)
import qualified Data.Tree       as Tree

type Heightmap = Map Pos Int
type Pos       = (Int, Int)

heightmapP :: Parser Heightmap
heightmapP = do
    xss <- endBy1 (many (readP $ (:[]) <$> digit)) (char '\n')
    let xss' = zipWith (\y hs -> zipWith (\x h -> ((x, y), h)) [0..] hs) [0..] xss
    let xs'  = concat xss'
    pure $ Map.fromList xs'

isLowPoint :: Heightmap -> Pos -> Bool
isLowPoint m (x, y) =
    case m !? (x, y) of
      Just height -> all (> height) . mapMaybe (m !?) $ adjacents (x, y)
      Nothing     -> False

adjacents :: Pos -> [Pos]
adjacents (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

toGraph :: Heightmap -> (Graph, Vertex -> ((), Pos, [Pos]), Pos -> Maybe Vertex)
toGraph m = Graph.graphFromEdges
    [ ((), p, adjs)
    | p <- filter ((/= Just 9) . (m !?)) $ Map.keys m
    , let adjs = filter (\p' -> let mh = m !? p'
                                 in isJust mh && mh /= Just 9)
                        (adjacents p)
    ]

solution :: Heightmap :=> Int
solution = simpleSolution
    (fromParsec heightmapP)
    (sum . map ((+1) . snd) . Map.toList .
        (\m -> Map.filterWithKey (flip . const $ isLowPoint m) m))
    (product . take 3 . reverse . sort . map (length . Tree.flatten) .
        Graph.components . (\(g, _, _) -> g) . toGraph)
