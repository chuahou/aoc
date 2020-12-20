-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day20 (solution) where

import           Control.Monad ((>=>))
import           Data.Foldable (asum)
import           Data.List     (transpose, (\\))
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe    (isJust)

import           AOC.Parsec
import           AOC.Solution

----- SOLUTION -----

data Tile     = Tile Int [[Cell]]
data Cell     = Hash | Dot deriving Eq
type Assembly = Map (Int, Int) Tile
type Image    = Map (Int, Int) Cell

instance Show Tile where
    show (Tile n cs) = unlines . ("Tile " <> show n :) . map (concatMap show) $ cs

instance Eq Tile where
    Tile x _ == Tile y _ = x == y

instance Show Cell where
    show Hash = "#"
    show Dot  = "."

assemble :: [Tile] -> Maybe Assembly
assemble ts = go ts ts (0, 0) Map.empty
    where
        go :: [Tile] -> [Tile] -> (Int, Int) -> Assembly -> Maybe Assembly
        go []      _         _ placed = Just placed
        go _       []        _ _      = Nothing
        go toPlace (t:toTry) (x, y) placed =
            let addable      = filter canAdd $ tileTransforms t
                canAdd t'    =  checkRel (`canAddRight` t') (x - 1, y)
                             && checkRel (`canAddBelow` t') (x, y - 1)
                checkRel f p = maybe True f $ placed Map.!? p
                toPlace'     = toPlace \\ [t]
                nextPos | x == 11   = (0, y + 1)
                        | otherwise = (x + 1, y)
             in (asum . map (\t' -> go toPlace' toPlace' nextPos
                                       (Map.insert (x, y) t' placed)) $ addable)
             <|> go toPlace toTry (x, y) placed

        tileTransforms :: Tile -> [Tile]
        tileTransforms (Tile i cs) = map (Tile i) $ rots ++ map flip' rots
            where
                rotate = map reverse . transpose
                flip'  = map reverse
                rots   = take 4 . iterate rotate $ cs

        canAddRight :: Tile -> Tile -> Bool
        canAddRight (Tile _ t) (Tile _ t') = border == border' && isJust border
            where
                border  = mapM last t
                border' = mapM head t'

        canAddBelow :: Tile -> Tile -> Bool
        canAddBelow (Tile _ t) (Tile _ t') = border == border' && isJust border
            where
                border  = last t
                border' = head t'

assembleImage :: Assembly -> Image
assembleImage = Map.foldrWithKey insertTile Map.empty
    where
        insertTile :: (Int, Int) -> Tile -> Image -> Image
        insertTile (tx, ty) (Tile _ css) =
            Map.union (Map.fromList . offset tx ty . coords . trim $ css)

        offset :: Int -> Int -> [((Int, Int), Cell)] -> [((Int, Int), Cell)]
        offset tx ty = map (\((x, y), c) -> ((x + tx * 8, y + ty * 8), c))

        coords :: [[Cell]] -> [((Int, Int), Cell)]
        coords = concatMap (uncurry (\i -> zipWith (\j -> ((j, i),)) [0..]))
               . zip [0..]

        trim :: [[Cell]] -> [[Cell]]
        trim = trim' . map trim'
            where
                trim' = take 8 . drop 1

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

solution :: Assembly :=> Maybe Int
solution = simpleSolution
    (fromParsec (sepBy1 tileP endOfLine <* eof) >=> assemble)
    (fmap (product . map getId) . getCorners)
    undefined -- part2
    where
        getId (Tile x _) = x
        getCorners as    = mapM (as Map.!?) [ (x, y)
                                            | x <- [0, 11]
                                            , y <- [0, 11]
                                            ]
