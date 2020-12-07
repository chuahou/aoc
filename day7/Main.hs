-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import           Control.Monad   (forM_)
import qualified Data.Graph      as G
import qualified Data.Map.Strict as M

import           Lex             (tokens)
import           Parse           (Rule (..), parse)

makeGraph :: [Rule] ->
             (G.Graph, G.Vertex -> (String, String, [String]),
             String -> Maybe G.Vertex)
makeGraph = G.graphFromEdges . map makeEdge
    where makeEdge (Rule b bs) = (b, b, map snd bs)

makeMap :: [Rule] -> M.Map String [(Int, String)]
makeMap = M.fromList . map mapping
    where mapping (Rule b bs) = (b, bs)

part1 :: [Rule] -> Int
part1 rs = let (g, _, k2v) = makeGraph rs
               g' = G.transposeG g
               b  = case k2v "shinygold" of
                      Just v  -> v
                      Nothing -> error "graph does not contain shiny gold bag"
            in flip (-) 1 . length . G.reachable g' $ b

part2 :: [Rule] -> Int
part2 = flip (-) 1 . flip part2' "shinygold" . makeMap

part2' :: M.Map String [(Int, String)] -> String -> Int
part2' m s = case M.lookup s m of
               Just bs -> 1 + (sum . map (\(n, s') -> n * part2' m s') $ bs)
               Nothing -> error $ "can't find bag colour " <> s

main :: IO ()
main = do { input <- readFile "input"
          ; forM_ [part1, part2] (\f -> print . f . parse . tokens $ input)
          }
