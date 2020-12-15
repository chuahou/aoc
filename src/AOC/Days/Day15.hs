-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day15 (solution) where

import           Control.Monad               (forM_)
import           Control.Monad.ST
import           Data.Foldable               (foldlM)
import           Data.List.NonEmpty          (NonEmpty (..))
import qualified Data.List.NonEmpty          as NE
import qualified Data.Vector.Unboxed.Mutable as VM

import           AOC.Parsec
import           AOC.Solution

run :: NonEmpty Int -> Int -> Maybe Int
run input target
    | target <= 0            = Nothing
    | target <= length input = Just $ input NE.!! (target - 1)
    | otherwise = let len = length input
                   in Just $ runST $ do
        { v <- VM.replicate (maximum (target : NE.toList input) + 1) 0
        ; forM_ (zip (NE.init input) [1..]) $ uncurry (VM.write v)
        ; foldlM (speakNum v) (NE.last input) [len..target-1]
        }

speakNum :: VM.MVector s Int -> Int -> Int -> ST s Int
speakNum !v !prev i = do
    { prevPos <- VM.unsafeRead v prev
    ; VM.write v prev i
    ; return $ if prevPos == 0 then 0 else i - prevPos
    }

solution :: NonEmpty Int :=> Maybe Int
solution = simpleSolution
    (fromParsec $ nonEmptyP $ sepBy1 (readP (many1 digit)) (char ','))
    (`run` 2020)
    (`run` 30000000)
