-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day19 (solution) where

import           Control.Monad                (void)
import           Data.Bifunctor               (first)
import           Data.IntMap                  (IntMap)
import qualified Data.IntMap                  as IntMap
import           Text.ParserCombinators.ReadP

import qualified AOC.Parsec                   as P
import           AOC.Solution

----- SOLUTION -----

data Rule = NonTerminal RuleKey [[RuleKey]]
          | Terminal    RuleKey Char
    deriving (Show, Eq)

type RuleKey = Int

ruleParsers :: [Rule] -> IntMap (ReadP ())
ruleParsers rs = IntMap.fromList . map mkParser $ rs
    where
        mkParser (Terminal    k c)   = (k, void $ char c)
        mkParser (NonTerminal k kss) = (k, choice . map nonTermP $ kss)
        nonTermP = mapM_ (`getRuleParser` ruleParsers rs)

getRuleParser :: RuleKey -> IntMap (ReadP ()) -> ReadP ()
getRuleParser = IntMap.findWithDefault pfail

countMatching :: [Rule] -> [String] -> Int
countMatching rs = length . filter matches
    where
        p = 0 `getRuleParser` ruleParsers rs
        matches = not . null . readP_to_S (p >> eof)

----- PARSING -----

ruleKeyP :: P.Parser RuleKey
ruleKeyP = P.readP (P.many1 P.digit)

ruleP :: P.Parser Rule
ruleP = flip ($) <$> (ruleKeyP <* P.string ": ") <*> restP

restP :: P.Parser (RuleKey -> Rule)
restP = P.try unionP P.<|> termP
    where
        unionP =   P.sepBy1 keysP (P.string " | ")
               >>= \rs -> return (`NonTerminal` rs)
        keysP  =   P.sepBy1 ruleKeyP
                            (P.try $ P.char ' ' >> P.lookAhead ruleKeyP)
        termP  =   (P.char '"' *> P.anyChar <* P.char '"')
               >>= \c -> return (`Terminal` c)

inputP :: P.Parser ([Rule], [String])
inputP = (,) <$> P.endBy1 ruleP P.endOfLine <* P.endOfLine
             <*> P.endBy1 (P.many $ P.noneOf "\n") P.endOfLine

----- SKELETON -----

solution :: ([Rule], [String]) :=> Int
solution = simpleSolution
    (fromParsec inputP)
    (uncurry countMatching)
    (uncurry countMatching . first (map changeRule))
    where
        changeRule (NonTerminal 8  [[42]]) = NonTerminal 8 [[42], [42, 8]]
        changeRule (NonTerminal 11 [[42, 31]]) =
            NonTerminal 11 [[42, 31], [42, 11, 31]]
        changeRule other = other
