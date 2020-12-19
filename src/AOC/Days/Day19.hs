-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day19 (solution) where

import           Control.Monad (void)
import           Data.Either   (isRight)
import           Data.IntMap   (IntMap)
import qualified Data.IntMap   as IntMap

import           AOC.Parsec
import           AOC.Solution

----- SOLUTION -----

data Rule = NonTerminal RuleKey [[RuleKey]]
          | Terminal    RuleKey Char
    deriving (Show, Eq)

type RuleKey = Int

ruleParsers :: [Rule] -> IntMap (Parser ())
ruleParsers rs = IntMap.fromList . map mkParser $ rs
    where
        mkParser (Terminal    k c)   = (k, void $ char c)
        mkParser (NonTerminal k kss) = (k, choice . map (try . nonTermP) $ kss)
        nonTermP ks = case mapM (`IntMap.lookup` ruleParsers rs) ks of
                   Just ps -> sequence_ ps
                   Nothing -> fail "invalid nonterminal"

countMatching :: [Rule] -> [String] -> Maybe Int
countMatching rs ss = do
    { p <- 0 `IntMap.lookup` ruleParsers rs
    ; return . length . filter (isRight . parse (p >> eof) "") $ ss
    }

----- PARSING -----

ruleKeyP :: Parser RuleKey
ruleKeyP = readP (many1 digit)

ruleP :: Parser Rule
ruleP = do flip ($) <$> (ruleKeyP <* string ": ") <*> restP

restP :: Parser (RuleKey -> Rule)
restP = try unionP <|> termP
    where
        unionP =   sepBy1 keysP (string " | ")
               >>= \rs -> return (`NonTerminal` rs)
        keysP  =   sepBy1 ruleKeyP (try $ char ' ' >> lookAhead ruleKeyP)
        termP  =   (char '"' *> anyChar <* char '"')
               >>= \c -> return (`Terminal` c)

inputP :: Parser ([Rule], [String])
inputP = (,) <$> endBy1 ruleP endOfLine <* endOfLine
             <*> endBy1 (many $ noneOf "\n") endOfLine

----- SKELETON -----

solution :: ([Rule], [String]) :=> Maybe Int
solution = simpleSolution
    (fromParsec inputP)
    (uncurry countMatching)
    undefined
