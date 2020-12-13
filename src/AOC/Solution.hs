-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Solution ( (:=>) (Solution, Script)
                    , simpleSolution
                    , runSolution
                    ) where

import           System.IO      (FilePath)
import           System.Process (callProcess)

import           Paths_aoc

data a :=> b where
    Solution :: { parse    :: String -> Maybe a
                , part1    :: a -> b
                , part2    :: a -> b
                , printSol :: b -> String
                } -> a :=> b
    Script :: FilePath -> a :=> b

simpleSolution :: Show b
               => (String -> Maybe a) -> (a -> b) -> (a -> b) -> a :=> b
simpleSolution p p1 p2 = Solution p p1 p2 show

runSolution :: a :=> b -> String -> Maybe (IO ())
runSolution (Solution p p1 p2 ps) x = p x >>= \x' ->
    return . putStrLn . unlines . map (\part -> ps . part $ x') $ [p1, p2]
runSolution (Script s) _ = Just $ getDataFileName s >>= flip callProcess []
