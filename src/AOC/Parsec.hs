-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Parsec ( module Text.Parsec
                  , module Text.Parsec.String
                  , readP
                  , nonEmptyP
                  ) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Text.Parsec
import           Text.Parsec.String (Parser)

readP :: Read a => Parser String -> Parser a
readP = ((\cs -> case readMaybe cs of
                  Just x  -> return x
                  Nothing -> fail "readP") =<<)

nonEmptyP :: Parser [a] -> Parser (NonEmpty a)
nonEmptyP = ((\case
                x:xs -> return (x:|xs)
                _    -> fail "nonEmptyP") =<<)
