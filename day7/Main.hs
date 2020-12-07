-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

import qualified Data.Graph as G

import           Lex        (tokens)
import           Parse      (Rule (..), parse)

makeGraph :: [Rule] ->
             (G.Graph, G.Vertex -> (String, String, [String]),
             String -> Maybe G.Vertex)
makeGraph = G.graphFromEdges . map makeEdge
    where makeEdge (Rule b bs) = (b, b, map snd bs)

part1 :: [Rule] -> Int
part1 rs = let (g, _, k2v) = makeGraph rs
               g' = G.transposeG g
               b  = case k2v "shinygold" of
                      Just v  -> v
                      Nothing -> error "graph does not contain shiny gold bag"
            in flip (-) 1 . length . G.reachable g' $ b

main :: IO ()
main = readFile "input" >>= print . part1 . parse . tokens
