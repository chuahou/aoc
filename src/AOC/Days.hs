-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

{-# LANGUAGE TemplateHaskell #-}

module AOC.Days ( daySolutions
                , runSolution
                , formatDay
                ) where

import           Data.Maybe          (mapMaybe)
import           Language.Haskell.TH (Exp (..), Q, mkName)

import           AOC.Solution        (runSolution)

formatDay :: Int -> Maybe String
formatDay n
    | n >=  0 && n <  10 = Just $ '0' : show n
    | n >= 10 && n <= 25 = Just $       show n
    | otherwise          = Nothing

getDaySolution :: Int -> Maybe Exp
getDaySolution day = formatDay day
                   >>= \day' ->
                       Just . VarE . mkName $ "Day" <> day' <> ".solution"

daySolutions :: Q Exp
daySolutions = return . ListE . mapMaybe daySolution $ [1..25]

daySolution :: Int -> Maybe Exp
daySolution n = AppE (VarE $ mkName "runSolution") <$> getDaySolution n
