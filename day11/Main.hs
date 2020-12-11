-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou
--
-- Refactored solution below inspired by
-- https://github.com/mstksg/advent-of-code-2020

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

import           Control.Monad (forM_)
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Maybe    (fromMaybe, mapMaybe)

type ParsedInput = ([Pos], (Int, Int))
type Output      = Int

type Pos  = (Int, Int)
data Seat = Empty | Occupied deriving (Show, Eq)

type Neighbours = Map Pos [Pos]
type State      = Map Pos Seat

head' :: [a] -> Maybe a
head' []    = Nothing
head' (x:_) = Just x

fix :: Eq a => (a -> a) -> a -> a
fix f x = let x' = f x
           in if x == x' then x else fix f x'

neighbours :: [Pos] -> (Int, Int) -> Neighbours
neighbours ps (h, w) = Map.fromList $ map neighbours' ps
    where neighbours' (x, y) = ((x, y), [ (x', y')
                                        | x' <- [x-1..x+1]
                                        , x' >= 0 && x' < h
                                        , y' <- [y-1..y+1]
                                        , y' >= 0 && y' < w
                                        , x' /= x || y' /= y
                                        , (x', y') `elem` ps
                                        ])

initState :: [Pos] -> State
initState ps = Map.fromList $ map (, Empty) ps

step :: Int -> Neighbours -> State -> State
step threshold ns = fromMaybe (error "Failed to step") . step' threshold ns

step' :: Int -> Neighbours -> State -> Maybe State
step' threshold ns ss = Map.foldrWithKey c (Just Map.empty) ss
    where c :: Pos -> Seat -> Maybe State -> Maybe State
          c key st y = do
            { nsStates <-  filter (== Occupied)
                       .   mapMaybe (`Map.lookup` ss)
                       <$> Map.lookup key ns
            ; let st' = case st of
                          Occupied -> if length nsStates >= threshold
                                         then Empty
                                         else Occupied
                          Empty    -> if null nsStates
                                         then Occupied
                                         else Empty
            ; Map.insert key st' <$> y
            }

parse :: String -> Maybe ParsedInput
parse cs = do { let ls = lines cs
              ; let h  = length ls
              ; w  <- length <$> head' ls
              ; ss <- map fst
                   <$> (foldr filtSeat (Just []) . concat . enumerate) ls
              ; return (ss, (h, w))
              }
    where filtSeat (p, 'L') ys = ((p, 'L'):) <$> ys
          filtSeat (_, '.') ys = ys
          filtSeat _        _  = Nothing
          enumerate = zipWith (\i -> zip (map (i,) [0..])) [0..]

part1 :: ParsedInput -> Output
part1 (ps, (w, h)) = let ns = neighbours ps (w, h)
                         st = fix (step 4 ns) (initState ps)
                      in length . Map.filter (== Occupied) $ st

part2 :: ParsedInput -> Output
part2 = undefined

main :: IO ()
main = do { input <-  fromMaybe (error "Parse error") . parse
                  <$> readFile "input"
          ; forM_ [part1, part2] (\p -> print . p $ input)
          }
