-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day14 (solution) where

import           Data.Bits          (complement, (.&.), (.|.))
import           Data.Foldable      (foldl')
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe         (fromMaybe)
import           Text.Parsec        (digit, endOfLine, eof, many, many1, oneOf,
                                     parse, string, try)
import           Text.Parsec.String (Parser)
import           Text.Read          (readMaybe)

import           AOC.Solution

----- SOLUTION -----

type Value  = Integer
data Mask   = Mask { maskHigh :: Integer
                   , maskLow  :: Integer
                   } deriving Show
data Write  = Write Mask Int Value deriving Show
type Memory = Map Int Value

applyMask :: Mask -> Value -> Value
applyMask (Mask h l) = (.&.) (complement l) . (.|.) h

applyWrite :: Write -> Memory -> Memory
applyWrite (Write m addr val) = Map.insert addr (applyMask m val)

----- PARSING -----

maskP :: Parser Mask
maskP =   try (string "mask = ") >> many (oneOf "01X")
      >>= maybe (fail "toMask") return . toMask

toMask :: [Char] -> Maybe Mask
toMask = fmap (uncurry Mask) . foldl' s (Just (0, 0))
    where
        s (Just (h, l)) 'X' = Just (h * 2,     l * 2)
        s (Just (h, l)) '1' = Just (h * 2 + 1, l * 2)
        s (Just (h, l)) '0' = Just (h * 2,     l * 2 + 1)
        s _             _   = Nothing

writeP :: Parser (Int, Value)
writeP = (,) <$> (try (string "mem[") *> intP <* string "] = ")
             <*> (toInteger <$> intP)

intP :: Parser Int
intP = many1 digit >>= (\case
         Just x  -> return x
         Nothing -> fail "Invalid integer") . readMaybe

writeGroupP :: Parser [Write]
writeGroupP = do { mask <- maskP         <* endOfLine
                 ; xs   <- many1 (writeP <* endOfLine)
                 ; return $ map (uncurry $ Write mask) xs
                 }

inputP :: Parser [Write]
inputP = concat <$> many1 writeGroupP <* eof

----- SKELETON -----

solution :: [Write] :=> Value
solution = simpleSolution
    (fromParsec inputP)
    (sum . Map.elems . foldl' (flip applyWrite) Map.empty)
    undefined -- part2
