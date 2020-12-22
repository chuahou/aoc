-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day21 (solution) where

import           AOC.Parsec
import           AOC.Solution

import           Data.Bifunctor (bimap, second)
import           Data.Char      (isAlpha)
import           Data.List      (intersect, nub, sortOn, (\\))
import           Data.Maybe     (fromMaybe)

----- SOLUTION -----

type Ingredient = String
type Allergen   = String
type Food       = ([Ingredient], [Allergen])

concatUnzip :: [([a], [b])] -> ([a], [b])
concatUnzip = bimap concat concat . unzip

matchIngredients :: [Food] -> Maybe [(Allergen, Ingredient)]
matchIngredients fs = go . sort' $ zip allergens (map findMatches allergens)
    where
        allergens        = nub . snd . concatUnzip $ fs
        sort'            = sortOn (length . snd)
        findMatches a    = fromMaybe [] . foldr1 intersect
                         . map fst . filter (elem a . snd) $ fs
        go []            = Just []
        go ((a, [i]):xs) = ((a, i):) <$> go (sort' $ map (second (\\ [i])) xs)
        go _             = Nothing

noAllergens :: [Food] -> Maybe [Ingredient]
noAllergens fs = case matchIngredients fs of
                   Just matches -> Just $ filter (`notIn` matches)
                                          (fst . concatUnzip $ fs)
                   Nothing      -> Nothing
    where
        i `notIn` ms = i `notElem` concatMap (pure . snd) ms

----- PARSING -----

inputP :: Parser [Food]
inputP = endBy1 foodP endOfLine
    where
        foodP = (,) <$> endBy1 wordP (char ' ')
                    <*> between (string "(contains ") (char ')')
                                (sepBy1 wordP (string ", "))
        wordP = many1 (satisfy isAlpha)

----- SKELETON -----

solution :: [Food] :=> Maybe Int
solution = simpleSolution
    (fromParsec inputP)
    (fmap length . noAllergens)
    undefined -- part2
