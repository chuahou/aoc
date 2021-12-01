-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

{-# LANGUAGE TemplateHaskell #-}

module AOC.Days ( daySolutions
                , formatDay
                , Array
                , listArray
                , runSolution
                ) where

import           Data.Array          (Array, listArray)
import           Data.Maybe          (mapMaybe)
import           Language.Haskell.TH (Exp (..), Lit (..), Q, mkName)

import           AOC.Solution        (runSolution)

formatDay :: Int -> Maybe String
formatDay n
    | n >=  0 && n <  10 = Just $ '0' : show n
    | n >= 10 && n <= 25 = Just $       show n
    | otherwise          = Nothing

-- @listArray (1, length xs) xs@
-- where @xs = [runSolution Day01.solution, runSolution Day02.solution, ...]@
daySolutions :: Q Exp
daySolutions = let xs = ListE . mapMaybe daySolution $ [1..25]
                in return $ AppE
                    (AppE
                        (strVar "listArray")
                        (TupE . map pure $ [ LitE (IntegerL 1)
                                           , AppE (strVar "length") xs
                                           ])) xs
    where
        daySolution n = formatDay n >>= \n' -> Just $
            AppE (strVar "runSolution") (strVar $ "Day" <> n' <> ".solution")
        strVar = VarE . mkName
