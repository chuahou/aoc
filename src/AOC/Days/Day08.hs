-- SPDX-License-Identifier: MIT
-- Copyright (c) 2021 Chua Hou

module AOC.Days.Day08 (solution) where

import           AOC.Parsec
import           AOC.Solution
import           Data.Bifunctor  (first)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set        (Set)
import qualified Data.Set        as Set

data Segment = A | B | C | D | E | F | G deriving (Show, Eq, Ord)
type Pattern = Set Segment
type Entry   = ([Pattern], [Pattern])

entryP :: Parser Entry
entryP = do
    uniq <- count 10 (patternP <* char ' ') <* char '|'
    outs <- count 4  (char ' ' *> patternP)
    pure (uniq, outs)
    where
        patternP = Set.fromList <$> many1 segmentP
        segmentP = choice [ char 'a' >> pure A
                          , char 'b' >> pure B
                          , char 'c' >> pure C
                          , char 'd' >> pure D
                          , char 'e' >> pure E
                          , char 'f' >> pure F
                          , char 'g' >> pure G
                          ]

occurrences :: Segment -> Entry -> Int
occurrences s = length . filter (s `Set.member`) . fst

solve :: Entry -> Maybe (Map Segment Segment)
solve entry@(ps, _) = do
    b <- head $ fromOccurrences 6
    e <- head $ fromOccurrences 4
    f <- head $ fromOccurrences 9
    (ac1, ac2) <- case fromOccurrences 8 of [x, y] -> Just (x, y); _ -> Nothing
    (dg1, dg2) <- case fromOccurrences 7 of [x, y] -> Just (x, y); _ -> Nothing
    p1 <- head $ filter ((== 2) . Set.size) ps
    p4 <- head $ filter ((== 4) . Set.size) ps
    (a, c) <- if ac1 `Set.member` p1
                 then if not $ ac2 `Set.member` p1 then Just (ac2, ac1) else Nothing
                 else if       ac2 `Set.member` p1 then Just (ac1, ac2) else Nothing
    (d, g) <- if dg1 `Set.member` p4
                 then if not $ dg2 `Set.member` p4 then Just (dg1, dg2) else Nothing
                 else if       dg2 `Set.member` p4 then Just (dg2, dg1) else Nothing
    pure $ Map.fromList [(a, A), (b, B), (c, C), (d, D), (e, E), (f, F), (g, G)]
    where
        segments = [A, B, C, D, E, F, G]
        fromOccurrences n = filter ((== n) . (`occurrences` entry)) segments

fromPattern :: Map Segment Segment -> Pattern -> Maybe Int
fromPattern m p = (traverse (`Map.lookup` m) . Set.toList $ p) >>=
                    (`Map.lookup` patterns) . Set.fromList
    where
        patterns = Map.fromList . map (first Set.fromList) $
            [ ([A, B, C, E, F, G],    0)
            , ([C, F],                1)
            , ([A, C, D, E, G],       2)
            , ([A, C, D, F, G],       3)
            , ([B, C, D, F],          4)
            , ([A, B, D, F, G],       5)
            , ([A, B, D, E, F, G],    6)
            , ([A, C, F],             7)
            , ([A, B, C, D, E, F, G], 8)
            , ([A, B, C, D, F, G],    9)
            ]

fromPatterns :: Map Segment Segment -> [Pattern] -> Maybe Int
fromPatterns m = foldl' s (Just 0)
    where s y p = do { y' <- y; p' <- fromPattern m p; pure $ y' * 10 + p' }

solution :: [Entry] :=> Maybe Int
solution = simpleSolution
    (fromParsec $ endBy entryP (char '\n'))
    (pure . length . filter ((`elem` [2, 3, 4, 7]) . Set.size) . concatMap snd)
    (fmap sum . traverse (\e@(_, os) -> solve e >>= (`fromPatterns` os)))
