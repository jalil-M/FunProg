module StringAlignment where

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

string1 = "writers"
string2 = "vintner"

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = []
maximaBy valueFcn xs = [ value | value <- xs, valueFcn value == maximum (map valueFcn xs)]

score :: Char -> Char -> Int
score  x '-' = scoreSpace
score '-' y  = scoreSpace
score x y = if x == y then scoreMatch
            else scoreMismatch

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] ys = scoreSpace * length ys
similarityScore xs [] = scoreSpace * length xs
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + score x y , similarityScore xs (y:ys) + score x '-', similarityScore (x:xs) ys + score '-' y]
