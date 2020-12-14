-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day14 (solution) where

import           Data.Bits          (complement, (.&.), (.|.))
import           Data.Foldable      (foldl')
import           Data.List          (nub)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Text.Parsec        (char, digit, endOfLine, eof, many1, string,
                                     try, (<|>))
import           Text.Parsec.String (Parser)
import           Text.Read          (readMaybe)

import           AOC.Solution

----- SOLUTION -----

type Value    = Integer
type Mask     = NonEmpty MaskChar
data MaskChar = X | M1 | M0 deriving Show
data AddrChar = AX | A1 | A0 deriving Show
data Write    = Write Mask Int Value deriving Show
type Memory   = Map Int Value

valMask :: Mask -> (Integer, Integer)
valMask = foldl' s (0, 0)
    where
        s (h, l) X  = (h * 2,     l * 2)
        s (h, l) M1 = (h * 2 + 1, l * 2)
        s (h, l) M0 = (h * 2,     l * 2 + 1)

applyMask :: Mask -> Value -> Value
applyMask m = (.&.) (complement l) . (.|.) h
    where
        (h, l) = valMask m

addrMask :: Mask -> Int -> [Int]
addrMask m = nub . addrMask' . mask (NE.reverse m)
    where
        possible AX = [0, 1]
        possible A0 = [0]
        possible A1 = [1]
        addrMask' (x:|[])   = possible x
        addrMask' (x:|y:ys) = [ y' * 2 + x'
                              | x' <- possible x
                              , y' <- addrMask' (y:|ys)
                              ]
        mask (x:|[])   a = maskBit x a :| []
        mask (x:|y:ys) a = maskBit x a :| NE.toList (mask (y:|ys) (a `div` 2))
        maskBit X  _ = AX
        maskBit M0 x = if x `mod` 2 == 1 then A1 else A0
        maskBit M1 _ = A1

applyWrite :: Write -> Memory -> Memory
applyWrite (Write m addr val) = Map.insert addr (applyMask m val)

applyWrite' :: Write -> Memory -> Memory
applyWrite' (Write m addr val) =
    Map.union (Map.fromList (map (,val) (addrMask m addr)))

----- PARSING -----

maskP :: Parser Mask
maskP = try (string "mask = ") >> many1 maskCharP >>= toNonEmpty
    where
        maskCharP =   (char 'X' >> return X)
                  <|> (char '1' >> return M1)
                  <|> (char '0' >> return M0)
        toNonEmpty (x:xs) = return (x:|xs)
        toNonEmpty []     = fail "toNonEmpty"

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
    (sum . Map.elems . foldl' (flip applyWrite') Map.empty)
