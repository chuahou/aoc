-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day23 (solution) where

import           AOC.Solution

import           Control.Monad ((<=<))
import           Data.List     (sortOn)
import qualified Data.Vector   as V

-- 0th element is current cup; i-th element is the cup following the i-th cup
type Game = V.Vector Int

stepGame :: Int -> Int -> Game -> Maybe Game
stepGame _       0     g = Just g
stepGame maxCups iters g = do
    { curr     <- g V.!? 0
    ; pick1    <- g V.!? curr
    ; pick2    <- g V.!? pick1
    ; pick3    <- g V.!? pick2
    ; next     <- g V.!? pick3
    ; let dest =  checkDest (curr - 1) [pick1, pick2, pick3]
    ; dnext    <- g V.!? dest
    ; let g'   = g V.// [ (curr,  next)
                        , (pick3, dnext)
                        , (dest,  pick1)
                        , (0,     next)
                        ]
    ; stepGame maxCups (iters - 1) g'
    }
    where
        checkDest x ps
            | x <= 0      = checkDest maxCups ps
            | x `elem` ps = checkDest (x - 1) ps
            | otherwise   = x

solution :: [Int] :=> Maybe Int
solution = simpleSolution
    (Just . map (read . pure) . filter (/= '\n'))
    (showGame <=< stepGame 9       100      <=< mkGame)
    (getStars <=< stepGame 1000000 10000000 <=< mkGame . (<> [10..1000000]))
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
