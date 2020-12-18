-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day18 (solution) where

import           Control.Monad (void)

import           AOC.Parsec
import           AOC.Solution

data Expr = Atom Int | Add Expr Expr | Times Expr Expr deriving Show

wsP :: Parser ()
wsP = void $ many (char ' ')

exprP :: Parser Expr
exprP = chainl1 (termP <* wsP) (opP <* wsP)
    where
        termP =   Atom <$> readP (many1 digit)
              <|> char '(' *> exprP <* char ')'
        opP = (char '*' >> return Times) <|> (char '+' >> return Add)

exprP' :: Parser Expr
exprP' = chainl1 termP mulP
    where
        termP = chainl1 factorP addP <* wsP
        factorP =   Atom <$> readP (many1 digit)  <* wsP
                <|> char '(' *> exprP' <* char ')' <* wsP
        addP = char '+' <* wsP >> return Add
        mulP = char '*' <* wsP >> return Times

eval :: Expr -> Int
eval (Atom x)    = x
eval (Add x y)   = eval x + eval y
eval (Times x y) = eval x * eval y

solution :: String :=> Maybe Int
solution = simpleSolution
    Just
    (fmap (sum . map eval) . fromParsec (endBy1 exprP  endOfLine))
    (fmap (sum . map eval) . fromParsec (endBy1 exprP' endOfLine))
