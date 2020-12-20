-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day20 (solution) where

import           AOC.Parsec
import           AOC.Solution

----- SOLUTION -----

-- borders are E, S, W, N, with left-to-right in list = clockwise
data Tile   = Tile Int Border Border Border Border deriving Show
type Border = [Cell]
data Cell   = Hash | Dot deriving Eq

instance Show Cell where
    show Hash = "#"
    show Dot  = "."

(~=) :: Border -> Border -> Bool
x ~= y = x == y || reverse x == y

borders :: Tile -> [Border]
borders (Tile _ b1 b2 b3 b4) = [b1, b2, b3, b4]

canNeighbour :: Border -> Tile -> Bool
canNeighbour b t = any (b ~=) $ borders t

findCorners :: [Tile] -> [Tile]
findCorners ts = go ts
    where
        go [] = []
        go (t:ts')
            | (length . filter hasNeighbour . borders) t == 2 = t : go ts'
            | otherwise                                       = go ts'
        hasNeighbour b = (length . filter (canNeighbour b)) ts > 1

----- PARSING -----

cellP :: Parser Cell
cellP = (char '#' >> return Hash) <|> (char '.' >> return Dot)

tileP :: Parser Tile
tileP = do
    { num <- string "Tile " *> readP (many1 digit) <* char ':' <* endOfLine
    ; pic <- count 10 (count 10 cellP <* endOfLine)
    ; (e, s, w, n) <- case getBorders pic of
                        Just bs -> return bs
                        Nothing -> fail "invalid pic"
    ; return $ Tile num e s w n
    }
    where
        getBorders pic = do
            { e <- mapM last pic
            ; s <- last pic
            ; w <- reverse <$> mapM head pic
            ; n <- reverse <$> head pic
            ; return (e, s, w, n)
            }

----- SKELETON -----

solution :: [Tile] :=> Maybe Int
solution = simpleSolution
    (fromParsec (sepBy1 tileP endOfLine <* eof))
    (\ts ->
        case findCorners ts of
          [Tile a _ _ _ _, Tile b _ _ _ _, Tile c _ _ _ _, Tile d _ _ _ _] ->
              Just $ product [a, b, c, d]
          _ -> Nothing)
    undefined -- part2
