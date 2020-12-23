-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day23 (solution) where

import           AOC.Solution
import           Control.Monad ((<=<))
import           Data.List     (elemIndex)

type Cup  = Int
type Game = [Cup]

splitElem :: Eq a => a -> [a] -> Maybe ([a], [a])
splitElem x xs =   elemIndex x xs
               >>= \idx -> case splitAt idx xs of
                             (bef, _:aft) -> Just (bef, aft)
                             _            -> Nothing

stepGame :: Int -> Game -> Maybe Game
stepGame maxCup (curr:p1:p2:p3:rest) = go (curr - 1)
    where
        go x
          | x < 1                 = go maxCup
          | x `elem` [p1, p2, p3] = go (x - 1)
          | otherwise             =   splitElem x rest
                                  >>= \(bef, aft) ->
                                      Just $ bef <> (x:p1:p2:p3:aft) <> [curr]
stepGame _ _ = Nothing

solution :: Game :=> Maybe Int
solution = simpleSolution
    (Just . map (read . pure) . filter (/= '\n'))
    (showGame <=< (!? 100) . iterate' (stepGame 9))
    undefined
    where iterate' f x = case f x of
                           Just x' -> x : iterate' f x'
                           Nothing -> [x]
          showGame g =   splitElem 1 g
                     >>= \(bef, aft) -> readMaybe . concatMap show $ aft <> bef
