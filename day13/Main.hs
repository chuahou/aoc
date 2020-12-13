-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

import           Control.Applicative ((<|>))
import           Control.Monad       (forM_)
import           Data.Function       (on)
import           Data.List           (minimumBy)
import           Data.Maybe          (fromMaybe, mapMaybe)
import qualified Text.Parsec         as P
import           Text.Parsec.String  (Parser)
import           Text.Read           (readMaybe)

----- TYPES, HELPERS -----

data Bus = OutOfService | Bus Int
    deriving (Show, Eq)

ceilDiv :: Int -> Int -> Int
ceilDiv x y = (x + y - 1) `div` y

----- SOLUTION -----

firstBus :: Int -> [Bus] -> (Bus, Int)
firstBus x = minimumBy (compare `on` snd)
           . mapMaybe (\b -> (b,) <$> waitBus x b)

waitBus :: Int -> Bus -> Maybe Int
waitBus x (Bus b)      = Just $ (x `ceilDiv` b) * b - x
waitBus _ OutOfService = Nothing

----- PARSERS -----

intP :: Parser Int
intP = (readMaybe <$> P.many1 P.digit) >>= \case
    Just x  -> return x
    Nothing -> fail "Invalid integer"

busP :: Parser Bus
busP = (Bus <$> intP) <|> (P.char 'x' >> return OutOfService)

inputP :: Parser (Int, [Bus])
inputP = do { x  <- intP <* P.endOfLine
            ; bs <- P.sepBy busP (P.char ',')
            ; _  <- P.optional P.endOfLine >> P.eof
            ; return (x, bs)
            }

----- SKELETON -----

type ParsedInput = (Int, [Bus])
type Output      = Int

parse :: String -> Maybe ParsedInput
parse s = case P.parse inputP "" s of
            Right ys -> Just ys
            Left  _  -> Nothing

part1 :: ParsedInput -> Output
part1 (x, bs) = case firstBus x bs of
                  (Bus b, t)        -> b * t
                  (OutOfService, _) -> error "Invalid bus"

part2 :: ParsedInput -> Output
part2 = undefined

runFile :: String -> IO ()
runFile s = do { input <- fromMaybe (error "Parse error") . parse <$> readFile s
               ; forM_ [part1, part2] (\p -> print . p $ input)
               }

main :: IO ()
main = runFile "input"
