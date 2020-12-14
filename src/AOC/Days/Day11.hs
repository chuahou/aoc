-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day11 (solution) where

import           Data.Map        (Map, (!?))
import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)

import           AOC.Solution

type Pos   = (Int, Int)
type Sets  = Map Pos [Pos]
type State = Map Pos Seat
data Seat  = Empty | Occupied | Floor deriving Eq

fix :: Eq a => (a -> a) -> a -> a
fix f x = let x' = f x in if x == x' then x else fix f x'

step :: Int -> Sets -> State -> State
step threshold sets state = Map.mapWithKey step' state
    where
        step' pos Empty    = if Occupied `notElem` set pos
                                then Occupied else Empty
        step' pos Occupied = if not . null . drop (threshold - 1)
                                . filter (== Occupied) $ set pos
                                then Empty else Occupied
        step' _   Floor    = Floor
        set pos = maybe (error "Invalid seat") (mapMaybe (state !?))
                $ sets !? pos

makeSets :: (State -> Pos -> [Pos]) -> State -> Sets
makeSets f s = Map.mapWithKey (const . f s) s

neighbours :: State -> Pos -> [Pos]
neighbours s (x, y) = filter (`Map.member` s) [ (x + dx, y + dy)
                                              | (dx, dy) <- directions
                                              ]

visible :: State -> Pos -> [Pos]
visible s (x, y) = mapMaybe visible' directions
    where
        visible' (dx, dy) = visible'' (x + dx, y + dy) (dx, dy)
        visible'' (x', y') (dx, dy) =
            case s !? (x', y') of
              Just Occupied -> Just (x', y')
              Just Empty    -> Just (x', y')
              Just Floor    -> visible'' (x' + dx, y' + dy) (dx, dy)
              Nothing       -> Nothing

directions :: [Pos]
directions = [ (x, y) | x <- [-1..1] , y <- [-1..1] , (x, y) /= (0, 0) ]

parse :: String -> Maybe State
parse = fmap Map.fromList . mapM readSeat . enumerate . lines
    where
        enumerate = concat . zipWith (\i -> zip (map (i,) [0..])) [0..]
        readSeat (p, 'L') = Just (p, Empty)
        readSeat (p, '.') = Just (p, Floor)
        readSeat (_, _)   = Nothing

solution :: State :=> Int
solution = simpleSolution
    parse
    (solve 4 neighbours)
    (solve 5 visible)
    where
        solve n f s = Map.size . Map.filter (== Occupied)
                    . fix (step n (makeSets f s)) $ Map.filter (/= Floor) s
