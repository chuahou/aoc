-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

module AOC.Days.Day12 (solution) where

import           Data.Foldable      (foldl')
import qualified Text.Parsec        as P
import           Text.Parsec.String (Parser)
import           Text.Read          (readMaybe)

import           AOC.Solution

----- TYPES, HELPERS -----

data Instruction = MoveDir Direction Int | L | R | LL | F Int
    deriving (Show, Eq)

type Pos   = (Int, Int)
data State = State Pos Direction

type Direction = Pos

initState :: State
initState = State (0, 0) (1, 0)

initState' :: State
initState' = State (0, 0) (10, 1)

moveDir :: Direction -> Int -> Pos -> Pos
moveDir (dx, dy) l (x, y) = (x + l * dx, y + l * dy)

manhattan :: State -> Int
manhattan (State (x, y) _) = abs x + abs y

----- SOLUTION -----

step :: Instruction -> State -> State
step (MoveDir dir len) (State pos dir')   = State (moveDir dir len pos) dir'
step (F len)           (State pos dir)    = State (moveDir dir len pos) dir
step L                 (State pos (x, y)) = State pos (-y,  x)
step R                 (State pos (x, y)) = State pos ( y, -x)
step LL                (State pos (x, y)) = State pos (-x, -y)

step' :: Instruction -> State -> State
step' (MoveDir dir len) (State pos dir') = State pos (moveDir dir len dir')
step' i                 s                = step i s

run :: (Instruction -> State -> State) -> [Instruction] -> State -> State
run f xs y = foldl' (flip f) y xs

----- PARSERS -----

dirP :: Parser Direction
dirP = P.choice [ P.char 'E' >> return ( 1,  0)
                , P.char 'S' >> return ( 0, -1)
                , P.char 'W' >> return (-1,  0)
                , P.char 'N' >> return ( 0,  1)
                ]

intP :: Parser Int
intP = do { cs <- P.many1 P.digit
          ; case readMaybe cs of
              Just x  -> return x
              Nothing -> fail "Invalid integer"
          }

instructionP :: Parser Instruction
instructionP = P.choice [ MoveDir <$> dirP <*> intP
                        , P.try (P.string "L90")  >> return L
                        , P.try (P.string "R90")  >> return R
                        , P.try (P.string "L180") >> return LL
                        , P.try (P.string "R180") >> return LL
                        , P.try (P.string "L270") >> return R
                        , P.try (P.string "R270") >> return L
                        , P.char 'F' >> F <$> intP
                        ]

solution :: [Instruction] :=> Int
solution = simpleSolution
    (fromParsec (P.many (instructionP <* P.endOfLine)))
    (manhattan . flip (run step)  initState)
    (manhattan . flip (run step') initState')
