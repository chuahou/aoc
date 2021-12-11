-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day09 (solution) where

import           Prelude         hiding ((!?))

import           AOC.Parsec
import           AOC.Solution
import           Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)

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
      Just height -> all (> height) . mapMaybe (m !?) $ [ (x - 1, y)
                                                        , (x + 1, y)
                                                        , (x,     y - 1)
                                                        , (x,     y + 1)
                                                        ]
      Nothing     -> False

solution :: Heightmap :=> Int
solution = simpleSolution
    (fromParsec heightmapP)
    (sum . map ((+1) . snd) . Map.toList .
        (\m -> Map.filterWithKey (flip . const $ isLowPoint m) m))
    undefined -- part2
