-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou
--
-- Refactored solution below inspired by
-- https://github.com/mstksg/advent-of-code-2020

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

import           Control.Monad   (forM_)
import           Data.Map        (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (fromMaybe, mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as Set

type ParsedInput = (Set Pos, (Int, Int))
type Output      = Int

type Pos  = (Int, Int)
data Seat = Empty | Occupied deriving (Show, Eq)

type Neighbours = Map Pos (Set Pos)
type State      = Map Pos Seat

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

fix :: Eq a => (a -> (a, Bool)) -> a -> a
fix f x = let (x', b) = f x
           in if b then fix f x' else x

neighbours :: Set Pos -> (Int, Int) -> Neighbours
neighbours ps (h, w) = Map.fromList . Set.toList $ Set.map neighbours' ps
    where neighbours' (x, y) = ((x, y), Set.fromList [ (x', y')
                                                     | x' <- [x-1..x+1]
                                                     , x' >= 0 && x' < h
                                                     , y' <- [y-1..y+1]
                                                     , y' >= 0 && y' < w
                                                     , x' /= x || y' /= y
                                                     , (x', y') `Set.member` ps
                                                     ])

visible :: Set Pos -> (Int, Int) -> Neighbours
visible ps (h, w) = Map.fromList . Set.toList $ Set.map visible' ps
    where visible' (x, y) = ( (x, y)
                            , Set.fromList . mapMaybe (visible'' (x, y)) $ dirns
                            )
          dirns = [ (dx, dy)
                  | dx <- [-1..1]
                  , dy <- [-1..1]
                  , dx /= 0 || dy /= 0
                  ]
          visible'' (x', y') (dx, dy)
            | x' < 0 || x' > h || y' < 0 || y' > w = Nothing
            | (x' + dx, y' + dy) `Set.member` ps   = Just (x'+dx, y'+dy)
            | otherwise                            = visible'' (x'+dx, y'+dy)
                                                               (dx, dy)

initState :: Set Pos -> State
initState = Map.fromSet (const Empty)

step :: Int -> Neighbours -> State -> (State, Bool)
step threshold ns = fromMaybe (error "Failed to step") . step' threshold ns

step' :: Int -> Neighbours -> State -> Maybe (State, Bool)
step' threshold ns ss = Map.foldrWithKey c (Just (Map.empty, False)) ss
    where c key st mb = do
            { (m, b) <- mb
            ; ns'    <- Map.lookup key ns
            ; let nsStates = Map.filter (== Occupied)
                           $ Map.intersection ss (Map.fromSet id ns')
            ; let (st', b') = case st of
                                Occupied -> if length nsStates >= threshold
                                               then (Empty, True)
                                               else (Occupied, b)
                                Empty    -> if null nsStates
                                               then (Occupied, True)
                                               else (Empty, b)
            ; return (Map.insert key st' m, b || b')
            }

parse :: String -> Maybe ParsedInput
parse cs = do { let ls = lines cs
              ; let h  = length ls
              ; w  <- length <$> head' ls
              ; ss <- Set.map fst
                   <$> (foldr filtSeat (Just Set.empty) . concat . enumerate) ls
              ; return (ss, (h, w))
              }
    where filtSeat (p, 'L') ys = Set.insert (p, 'L') <$> ys
          filtSeat (_, '.') ys = ys
          filtSeat _        _  = Nothing
          enumerate = zipWith (\i -> zip (map (i,) [0..])) [0..]

part1 :: ParsedInput -> Output
part1 (ps, (w, h)) = let ns = neighbours ps (w, h)
                         st = fix (step 4 ns) (initState ps)
                      in length . Map.filter (== Occupied) $ st

part2 :: ParsedInput -> Output
part2 (ps, (w, h)) = let ns = visible ps (w, h)
                         st = fix (step 5 ns) (initState ps)
                      in length . Map.filter (== Occupied) $ st

main :: IO ()
main = do { input <-  fromMaybe (error "Parse error") . parse
                  <$> readFile "input"
          ; forM_ [part1, part2] (\p -> print . p $ input)
          }
