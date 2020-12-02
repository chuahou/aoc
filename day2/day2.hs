-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import           Data.Either        (fromRight)
import           Text.Parsec
import           Text.Parsec.String (Parser)

data Requirement = Requirement { lowBound  :: Int
                               , highBound :: Int
                               , character :: Char
                               , password  :: String
                               }

solve1 :: [String] -> Int
solve1 = length . filter (fromRight False . parse (checkPassword <$> lineP) "")

lineP :: Parser Requirement
lineP = do { low  <- read <$> many1 digit <* char '-'
           ; high <- read <$> many1 digit <* space
           ; c    <- anyChar              <* string ": "
           ; cs   <- many anyChar         <* eof
           ; return $ Requirement low high c cs
           }

checkPassword :: Requirement -> Bool
checkPassword (Requirement low high c cs) =
    let cCount = length . filter (== c) $ cs
     in cCount <= high && cCount >= low

main :: IO ()
main = do { input <- readFile "input"
          ; print . solve1 . lines $ input
          }
