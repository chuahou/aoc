-- SPDX-License-Identifier: MIT
-- Copyright (c) 2020 Chua Hou

group :: String -> [String]
group = group' ""

group' :: String -> String -> [String]
group' ys []             = [reverse ys]
group' ys ('\n':'\n':xs) = reverse ys : group' "" xs
group' ys (x:xs)         = group' (x:ys) xs

count :: [String] -> Int
count (cs:css) = length . foldr (filter . flip elem) cs $ css
count []       = 0

main :: IO ()
main = readFile "input" >>= print . sum . map (count . lines) . group
