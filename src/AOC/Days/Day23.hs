-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day23 (solution) where

import           AOC.Solution

import           Data.List.NonEmpty (NonEmpty (..))

type Cup  = Int
type Game = (Cup, Cup, Cup, Cup, NonEmpty Cup) -- at least 5 cups

rotate :: Game -> Game
rotate (a, b, c, d, e:|f:fs) = (b, c, d, e, f :| fs <> [a])
rotate (a, b, c, d, e:|[])   = (b, c, d, e, a :| [])

stepGame :: Game -> Game
stepGame (curr, p1, p2, p3, cups) = rotate $ tryDest (curr - 1)
    where
        tryDest x
            | x < minimum' cups     = tryDest $ maximum' cups
            | x `elem` [p1, p2, p3] = tryDest $ x - 1
            | otherwise             = go x ([], cups)
        go x (skipped, y:|ys)
            | x == y = case skipped of
                         []             -> (curr,  y, p1, p2, p3:|ys)
                         [s1]           -> (curr, s1,  y, p1, p2:|p3:ys)
                         [s1, s2]       -> (curr, s1, s2,  y, p1:|p2:p3:ys)
                         [s1, s2, s3]   -> (curr, s1, s2, s3,  y:|p1:p2:p3:ys)
                         s1:s2:s3:s4:ss -> (curr, s1, s2, s3, s4:|rest)
                            where rest = ss <> (y:p1:p2:p3:ys)
            | otherwise = case ys of
                            (z:zs) -> go x (skipped <> [y], z:|zs)
                            []     -> tryDest $ x - 1

solution :: Game :=> Game
solution = Solution
    (mkGame . map (read . pure) . filter (/= '\n'))
    ((!! 100) . iterate stepGame)
    undefined -- part2
    showGame
    where
        mkGame (a:b:c:d:e:fs) = Just (a, b, c, d, e:|fs)
        mkGame _              = Nothing
        showGame (1, b, c, d, e:|fs) = concatMap show (b:c:d:e:fs)
        showGame g                   = showGame $ rotate g
