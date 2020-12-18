-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day18 (solution) where

import           Control.Monad (void)

import           AOC.Parsec
import           AOC.Solution

data Expr = Atom Int | Add Expr Expr | Times Expr Expr deriving Show

wsP :: Parser ()
wsP = void $ many (char ' ' <|> endOfLine)

exprP :: Parser Expr
exprP = chainl1 (termP <* wsP) (opP <* wsP)
    where
        termP =   Atom <$> readP (many1 digit)
              <|> char '(' *> exprP <* char ')'
        opP = (char '*' >> return Times) <|> (char '+' >> return Add)

eval :: Expr -> Int
eval (Atom x)    = x
eval (Add x y)   = eval x + eval y
eval (Times x y) = eval x * eval y

solution :: [Expr] :=> Int
solution = simpleSolution
    (fromParsec $ many exprP)
    (sum . map eval)
    undefined -- part2
