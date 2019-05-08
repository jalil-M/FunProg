module StringAlignment where

{- Smith-Waterman scoring penalties -}
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

{- Needleman-Wunsch scoring penalties
scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2
-}

string1 = "writers"
string2 = "vintner"

{- Appends the prefix (first char) in a list of tuples -}
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

{- Appends the second char in a list of tuples for left-to-right ordering -}
attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs++[h1], ys++[h2]) | (xs,ys) <- aList]

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = []
maximaBy valueFcn xs = [ value | value <- xs, valueFcn value == maximum (map valueFcn xs)]

{- .. where the local optimal alignment between xs and ys is determined by the resulting column score -}
score :: Char -> Char -> Int
score  x '-' = scoreSpace
score '-' y  = scoreSpace
score x y
    | x == y = scoreMatch
    | otherwise = scoreMismatch

{- Caclulate similarity score for two non-empty strings .. -}
-- WITHOUT memoization, to be optimized in future implementation
similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] ys = scoreSpace * length ys
similarityScore xs [] = scoreSpace * length xs
-- where first element is the similarity score, second is the optimal alignment pair
similarityScore (x:xs) (y:ys) = maximum [ similarityScore xs ys + score x y, 
                                          similarityScore xs (y:ys) + score x '-', 
                                          similarityScore (x:xs) ys + score '-' y ]


{- The new similarity Score Function using the definition of mcsLength (memoization technique) -}
similarityScoreNew :: String -> String -> Int
similarityScoreNew xs ys = sLen (length xs) (length ys)
  where
    sLen i j = sTable!!i!!j
    sTable = [[ sEntry i j | j<-[0..]] | i<-[0..] ]

    sEntry :: Int -> Int -> Int
    -- initialize entry to zero
    -- iterate over similarityScore 
    sEntry i 0 = scoreSpace * i
    sEntry 0 j = scoreSpace * j
    sEntry i j = maximum [sLen (i-1) (j-1) + score x y,sLen i (j-1) + score x '-',sLen (i-1) j + score '-' y]
      where
         x = xs!!(i-1)
         y = ys!!(j-1)                                    

{- Element type for storing intermediate table calculations -}
type AlignmentType = (String,String)
optAlignments :: String -> String -> [AlignmentType]


{- Return list of all optimal allignments between string1 and string2 -}
-- WITHOUT memoization, to be optimized in future implementation
optAlignments [] [] = [([], [])]
-- traverse string1 for matching char x then substring xs -}
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
-- traverse string2 for matching char y then substring ys -}
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
-- get unoptimized similarity score
optAlignments (x:xs) (y:ys) = maximaBy unoptimizedScoring $ concat[ 
    attachHeads x y (optAlignments xs ys),       {- Alignment #1 -}
	attachHeads x '-' (optAlignments xs (y:ys)), {- Alignment #2 -}
    attachHeads '-' y (optAlignments (x:xs) ys)  {- Alignment #3 -} ]
    where unoptimizedScoring (xs, ys) = sum $ zipWidth (curry score) xs ys

optAlignmentsTable :: String -> String -> [AlignmentType]
optAlignmentsTable xs ys = snd (optAlignment (length xs) (length ys))
    where optAlignment :: Int-> Int -> (Int, [AlignmentType])
          optAlignment i j = aTable!!i!!j
          aTable = [[tablePair i j | j <- [0..]] | i <- [0..]]

          -- checks if previous optimal path exists
          insertPair :: Int -> Int -> (Int, [AlignmentType]) -> (Int, [AlignmentType])
          insertPair (score x y) (simScorePrev optAlignPrev) = (score + simScorePrev, attachTails x y optAlignPrev)
          
          -- traceback table entry, tuple of (score, [direction])
          tablePair :: Int-> Int -> (Int, [AlignmentType])
          -- initialize first row, column score to zero
          tablePair 0 0 = (0, ["",""])
          tablePair i 0 = scoreSpace * i
          tablePair 0 j = scoreSpace * j
          tablePair i j
              | x == y
              | otherwise = (fst $ head optNeighbor, concat . map snd $ optNeighbor)
              where
                x = xs!!(i-1)
                y = ys!!(j-1)

                -- calculate neighoring cell scores
                diag = insertPair (scoreMismatch x y) (similarityScoreNew (i-1) (j-1))
                down = insertPair (scoreSpace '-' y) (similarityScoreNew i (j-1))
                left = insertPair (scoreSpace x '-') (similarityScoreNew (i-1) j)

                -- calculate highest candidate score
                optNeighbor = maximaBy fst [diag, down, left]

{- Print all optimal alignments between string1 and string2 -}
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
	putStrLn . "Optimal alignments: \n"
	{- print list here -}


