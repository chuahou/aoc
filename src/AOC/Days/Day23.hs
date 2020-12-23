-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day23 (solution) where

import           AOC.Solution

import           Control.Monad               ((<=<))
import           Control.Monad.ST            (ST, runST)
import           Data.List                   (sortOn)
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as VM

-- 0th element is current cup; i-th element is the cup following the i-th cup
type Game = V.Vector Int

stepGame :: Int -> Int -> Game -> Game
stepGame maxCups iters g = runST (V.thaw g >>= go iters >>= V.freeze)
    where
        go :: Int -> VM.MVector s Int -> ST s (VM.MVector s Int)
        go 0 v = pure v
        go i v = do
            { curr     <- v `VM.read` 0
            ; pick1    <- v `VM.read` curr
            ; pick2    <- v `VM.read` pick1
            ; pick3    <- v `VM.read` pick2
            ; next     <- v `VM.read` pick3
            ; let dest =  checkDest (curr - 1) [pick1, pick2, pick3]
            ; dnext    <- v `VM.read` dest
            ; VM.write v curr  next
            ; VM.write v pick3 dnext
            ; VM.write v dest  pick1
            ; VM.write v 0     next
            ; go (i - 1) v
            }
        checkDest x ps
            | x <= 0      = checkDest maxCups ps
            | x `elem` ps = checkDest (x - 1) ps
            | otherwise   = x

solution :: [Int] :=> Maybe Int
solution = simpleSolution
    (Just . map (read . pure) . filter (/= '\n'))
    (showGame . stepGame 9       100      <=< mkGame)
    (getStars . stepGame 1000000 10000000 <=< mkGame . (<> [10..1000000]))
    where
        showGame g = go (g V.!? 1) >>= readMaybe . concatMap show
            where
                go x =   x >>= \x' -> g V.!? x'
                     >>= \case
                         1 -> Just [x']
                         y -> (x':) <$> go (Just y)
        getStars g = do
            { s1 <- g V.!? 1
            ; s2 <- g V.!? s1
            ; return $ s1 * s2
            }
        mkGame (x:xs) = Just $ V.fromList . (x:) . map snd . sortOn fst
                      $ zip (x:xs) (xs <> [x])
        mkGame []     = Nothing
