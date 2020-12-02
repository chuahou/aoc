-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import           Control.Monad (forM_)
import           Data.Either   (fromRight)
import           Text.Parsec

type Requirement = (Int, Int, Char, String)

solve :: (Requirement -> Bool) -> [String] -> Int
solve p = length . filter (fromRight False . parse (p <$> lineP) "")

lineP :: Parsec String () Requirement
lineP = do { low  <- read <$> many1 digit <* char '-'
           ; high <- read <$> many1 digit <* space
           ; c    <- anyChar              <* string ": "
           ; cs   <- many anyChar         <* eof
           ; return (low, high, c, cs)
           }

check1 :: Requirement -> Bool
check1 (low, high, c, cs) =
    let cCount = length . filter (== c) $ cs
     in cCount <= high && cCount >= low

check2 :: Requirement -> Bool
check2 (x, y, c, cs) =
    max x y <= length cs &&
        let cx = cs !! (x - 1)
            cy = cs !! (y - 1)
         in (cx == c && cy /= c) || (cx /= c && cy == c)

main :: IO ()
main = do { input <- readFile "input"
          ; forM_ [check1, check2] (\p -> print . solve p . lines $ input)
          }
