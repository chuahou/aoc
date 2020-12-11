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
type State = Map Pos Seat
data Seat  = Empty | Occupied | Floor deriving Eq

fix :: Eq a => (a -> a) -> a -> a
fix f x = let x' = f x in if x == x' then x else fix f x'

step :: Int -> (State -> Pos -> [Seat]) -> State -> State
step threshold getSet state = Map.mapWithKey step' state
    where
        step' pos Empty    = if Occupied `notElem` getSet state pos
                                then Occupied else Empty
        step' pos Occupied = if not . null . drop (threshold - 1)
                                . filter (== Occupied) $ getSet state pos
                                then Empty else Occupied
        step' _   Floor    = Floor

neighbours :: State -> Pos -> [Seat]
neighbours s (x, y) = mapMaybe (s !?) [ (x + dx, y + dy)
                                      | (dx, dy) <- directions
                                      ]

visible :: State -> Pos -> [Seat]
visible s (x, y) = mapMaybe visible' directions
    where
        visible' (dx, dy) = visible'' (x + dx, y + dy) (dx, dy)
        visible'' (x', y') (dx, dy) =
            case s !? (x', y') of
              Just Occupied -> Just Occupied
              Just Empty    -> Nothing
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

part1 :: ParsedInput -> Output
part1 = Map.size . Map.filter (== Occupied) . fix (step 4 neighbours)

part2 :: ParsedInput -> Output
part2 = Map.size . Map.filter (== Occupied) . fix (step 5 visible)

solve :: String -> IO ()
solve s = do { input <-  fromMaybe (error "Parse error") . parse <$> readFile s
             ; forM_ [part1, part2] (\p -> print . p $ input)
             }

main :: IO ()
main = solve "input"
