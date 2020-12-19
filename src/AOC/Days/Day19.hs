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
        nonTermP = mapM_ (`getRuleParser` ruleParsers rs)

getRuleParser :: RuleKey -> IntMap (Parser ()) -> Parser ()
getRuleParser n = IntMap.findWithDefault (fail $ "no nonterminal " <> show n) n

countMatching :: [Rule] -> [String] -> Int
countMatching rs = length . filter (isRight . parse (p >> eof) "")
    where
        p = 0 `getRuleParser` ruleParsers rs

countMatching' :: [Rule] -> [String] -> Int
countMatching' rs = length . filter (isRight . parse (p >> eof) "")
    where
        p = do { y42 <- many1 (getRuleParser 42 $ ruleParsers rs)
               ; y31 <- many1 (getRuleParser 31 $ ruleParsers rs)
               ; if length y42 > length y31 then return () else fail "42 < 31"
               }
    -- assumes 0 -> 8 11
    --         8 -> 42 | 42 8
    --        11 -> 42 31 | 42 11 31
    -- (which is true for this problem)

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

solution :: ([Rule], [String]) :=> Int
solution = simpleSolution
    (fromParsec inputP)
    (uncurry countMatching)
    (uncurry countMatching')
