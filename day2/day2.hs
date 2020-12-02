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

solve :: (Requirement -> Bool) -> [String] -> Int
solve p = length . filter (fromRight False . parse (p <$> lineP) "")

lineP :: Parser Requirement
lineP = do { low  <- read <$> many1 digit <* char '-'
           ; high <- read <$> many1 digit <* space
           ; c    <- anyChar              <* string ": "
           ; cs   <- many anyChar         <* eof
           ; return $ Requirement low high c cs
           }

check1 :: Requirement -> Bool
check1 (Requirement low high c cs) =
    let cCount = length . filter (== c) $ cs
     in cCount <= high && cCount >= low

check2 :: Requirement -> Bool
check2 (Requirement x y c cs) =
    max x y <= length cs &&
        let cx = cs !! (x - 1)
            cy = cs !! (y - 1)
         in (cx == c && cy /= c) || (cx /= c && cy == c)

main :: IO ()
main = do { input <- readFile "input"
          ; print . solve check1 . lines $ input
          ; print . solve check2 . lines $ input
          }
