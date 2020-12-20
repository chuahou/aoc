-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day18 (solution) where

import           AOC.Parsec
import           AOC.Solution

data Expr = Atom Int | Add Expr Expr | Times Expr Expr deriving Show

lexeme :: Parser a -> Parser a
lexeme = (<* many (char ' '))

exprP :: Parser Expr
exprP = chainl1 (lexeme termP) (lexeme opP)
    where
        termP =   Atom <$> readP (many1 digit)
              <|> char '(' *> exprP <* char ')'
        opP = (char '*' >> return Times) <|> (char '+' >> return Add)

exprP' :: Parser Expr
exprP' = chainl1 (lexeme termP) (lexeme mulP)
    where
        termP = chainl1 (lexeme factorP) (lexeme addP)
        factorP =   Atom <$> readP (many1 digit)
                <|> char '(' *> exprP' <* char ')'
        addP = char '+' >> return Add
        mulP = char '*' >> return Times

eval :: Expr -> Int
eval (Atom x)    = x
eval (Add x y)   = eval x + eval y
eval (Times x y) = eval x * eval y

solution :: String :=> Maybe Int
solution = simpleSolution
    Just
    (fmap (sum . map eval) . fromParsec (endBy1 exprP  endOfLine))
    (fmap (sum . map eval) . fromParsec (endBy1 exprP' endOfLine))
