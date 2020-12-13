-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Solution ( (:=>) (Solution)
                    , simpleSolution
                    , runSolution
                    ) where

data a :=> b where
    Solution :: { parse    :: String -> Maybe a
                , part1    :: a -> b
                , part2    :: a -> b
                , printSol :: b -> String
                } -> a :=> b

simpleSolution :: Show b
               => (String -> Maybe a) -> (a -> b) -> (a -> b) -> a :=> b
simpleSolution p p1 p2 = Solution p p1 p2 show

runSolution :: a :=> b -> String -> Maybe String
runSolution (Solution p p1 p2 ps) x = p x >>= \x' ->
    return . unlines . map (\part -> ps . part $ x') $ [p1, p2]
