-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou
--
-- Re-refactored simpler solution below heavily inspired by
-- https://github.com/mstksg/advent-of-code-2020
-- https://github.com/pwm/aoc2020
--
-- See further back in commit history for all my terrible solutions prior to
-- checking these solutions.

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

import           Control.Monad   (forM_)
import           Data.Map        (Map, (!?))
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe, mapMaybe)

type ParsedInput = State
type Output      = Int

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

parse :: String -> Maybe ParsedInput
parse = fmap Map.fromList . mapM readSeat . enumerate . lines
    where
        enumerate = concat . zipWith (\i -> zip (map (i,) [0..])) [0..]
        readSeat (p, 'L') = Just (p, Empty)
        readSeat (p, '.') = Just (p, Floor)
        readSeat (_, _)   = Nothing

solve :: Int -> (State -> Pos -> [Pos]) -> State -> Int
solve n f s = Map.size . Map.filter (== Occupied)
            . fix (step n (makeSets f s)) $ Map.filter (/= Floor) s

part1 :: ParsedInput -> Output
part1 = solve 4 neighbours

part2 :: ParsedInput -> Output
part2 = solve 5 visible

runFile :: String -> IO ()
runFile s = do { input <- fromMaybe (error "Parse error") . parse <$> readFile s
               ; forM_ [part1, part2] (\p -> print . p $ input)
               }

main :: IO ()
main = runFile "input"
