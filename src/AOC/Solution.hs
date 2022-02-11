-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Solution ( (:=>) (Solution, Script, ForeignC)
                    , simpleSolution
                    , runSolution
                    , fromParsec
                    , test
                    ) where

import           Data.Maybe       (fromMaybe)
import           System.Process   (callProcess)

import           AOC.Parsec

import           Foreign.C.String (CString, withCString)
import           Paths_aoc

data a :=> b = Solution (String -> Maybe a) (a -> b) (a -> b) (b -> String)
             | Script FilePath
             | ForeignC (CString -> IO ())

simpleSolution :: Show b
               => (String -> Maybe a) -> (a -> b) -> (a -> b) -> a :=> b
simpleSolution p p1 p2 = Solution p p1 p2 show

runSolution :: a :=> b -> String -> Maybe (IO ())
runSolution (Solution p p1 p2 ps) x = p x >>= \x' ->
    return . putStrLn . unlines . map (\part -> ps . part $ x') $ [p1, p2]
runSolution (Script s) _ = Just $ getDataFileName s >>= flip callProcess []
runSolution (ForeignC f) x = Just $ withCString x f

fromParsec :: Parser a -> (String -> Maybe a)
fromParsec p = either (const Nothing) Just . parse p ""

test :: a :=> b -> FilePath -> IO ()
test s f = readFile f >>= fromMaybe (fail "Failed to run") . runSolution s
