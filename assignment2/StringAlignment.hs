-- Q1: Which parts of your code/functions were hardest to write and why? 
{- The hardest part to code was our `optAlignmentsTable` function. This is because the idea behind the optimal global alignment algorithm (Needleman-Wunsch) was difficult at first to grasp. We spent a large portion of our development schedule understanding the similarity matrix and scoring scheme implementations. -}

-- Q2: Which parts of your code/functions are you the most proud of?
{- For the reasons mentioned above, we are most proud of our `optAlignmentsTable` implementation. Performing the traceback process using the Needleman-Wunsch approach allows us to compute the similarityScore at a near-constant time complexity, which is a significant improvement over the previous rudimentary approach. -}

-- Q3: Which parts of the code/functions would you like to get a particular feedback on?
{- We would like to know what the difference in computational complexity would be in this context of global string alignment had we chosen to implement the Smith-Waterman approach. We would like to get feedback on the stylistic choices we made, using the various Haskell standard Prelude library functions. -}


module Main where
main :: IO ()
main = outputOptAlignments string1 string2

{- Smith-Waterman scoring penalties -}
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

{- Needleman-Wunsch scoring penalties
scoreMatch = 1
scoreMismatch = -1
scoreSpace = -2
-}

{- Appends the prefix (first char) in a list of tuples -}
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

{- Appends the second char in a list of tuples for left-to-right ordering -}
attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs++[h1], ys++[h2]) | (xs,ys) <- aList]

{- Generalization of the maximum function in two respects ("value" and all max elements) -}
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = []
maximaBy valueFcn xs = [ value | value <- xs, valueFcn value == maximum (map valueFcn xs)]

{- Element type for storing intermediate table calculations -}
type AlignmentType = (String,String)
optAlignments :: String -> String -> [AlignmentType]
{- Return list of all optimal allignments between string1 and string2 -}
-- WITHOUT memoization, optimized in optAlignmentsTable
optAlignments [] [] = [([], [])]
-- traverse string1 for matching char x then substring xs -}
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
-- traverse string2 for matching char y then substring ys -}
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
-- get unoptimized similarity score
optAlignments (x:xs) (y:ys) = maximaBy unoptimizedScoring $ concat[
    attachHeads x y (optAlignments xs ys),       {- Alignment #1 -}
    attachHeads x '-' (optAlignments xs (y:ys)), {- Alignment #2 -}
    attachHeads '-' y (optAlignments (x:xs) ys)  {- Alignment #3 -}]
    where unoptimizedScoring (xs, ys) = sum $ zipWith (curry score) xs ys

{- .. where the local optimal alignment between xs and ys is determined by the resulting column score -}
score :: (Char, Char) -> Int
score (x, '-') = scoreSpace
score ('-', y)  = scoreSpace
score (x, y)
    | x == y = scoreMatch
    | otherwise = scoreMismatch

{- Caclulate similarity score for two non-empty strings .. -}
-- WITHOUT memoization, optimized in similarityScoreNew
similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] ys = scoreSpace * length ys
similarityScore xs [] = scoreSpace * length xs
-- where first element is the similarity score, second is the optimal alignment pair
similarityScore (x:xs) (y:ys) = maximum [ similarityScore xs ys + score(x, y),
                                          similarityScore xs (y:ys) + score(x, '-'),
                                          similarityScore (x:xs) ys + score('-', y) ]

----------------------------------------------------------------------------------------------------------
--OPTIMIZATION OF THE CODE (PART 3)

{- The new similarity score function using the definition of mcsLength (memoization technique) -}
similarityScoreNew :: String -> String -> Int
similarityScoreNew xs ys = optAlignment (length xs) (length ys)
    where
    optAlignment i j = aTable!!i!!j
    aTable = [[ tablePair i j | j<-[0..]] | i<-[0..] ]

    tablePair :: Int -> Int -> Int
    tablePair i 0 = scoreSpace * i
    tablePair 0 j = scoreSpace * j
    tablePair i j
        | x == y = scoreMatch + optAlignment (i-1) (j-1)
        | otherwise = maximum [ optAlignment(i-1)(j-1) + scoreMismatch,
                                optAlignment i (j-1) + scoreSpace,
                                optAlignment (i-1) j + scoreSpace ]
        where
            x = xs!!(i-1)
            y = ys!!(j-1)


optAlignmentsTable :: String -> String -> [AlignmentType]
optAlignmentsTable xs ys = snd $ optAlignment (length xs) (length ys)
    where
        optAlignment i j = aTable!!i!!j
        aTable = [[tablePair i j | j <- [0..]] | i <- [0..]]

        -- checks if previous optimal path exists
        insertPair :: (Int, Char, Char) -> (Int, [AlignmentType]) -> (Int, [AlignmentType])
        insertPair (score, x, y) (simScorePrev, optAlignPrev) = (score + simScorePrev, attachTails x y optAlignPrev)

        -- traceback table entry, tuple of (score, [direction])
        tablePair :: Int-> Int -> (Int, [AlignmentType])
        -- initialize first row, column score to zero
        tablePair 0 0 = (0, [("","")])
        tablePair i 0 = (scoreSpace * i, [(take i xs, replicate i '-')])
        tablePair 0 j = (scoreSpace * j, [(replicate j '-', take j ys)])

        tablePair i j
            | x == y = insertPair (scoreMatch, x, y) (optAlignment (i-1) (j-1))
            | otherwise = (fst $ head optNeighbor, concatMap snd optNeighbor)
            where
                x = xs!!(i-1)
                y = ys!!(j-1)
                -- calculate neighoring cell scores
                diag = insertPair (scoreMismatch, x, y) (optAlignment (i-1) (j-1))
                down = insertPair (scoreSpace, '-', y) (optAlignment i (j-1))
                left = insertPair (scoreSpace, x, '-') (optAlignment (i-1) j)
                -- calculate highest candidate score
                optNeighbor = maximaBy fst [diag, down, left]

{- Testing the optAlignmentsTable method using memoization with a similarity scoring matrix -}
{- Ex1. Should return `3` optimal alignments -}
--string1 = "writers"
--string2 = "vintner"

{- Ex2. Should return `308` optimal alignments -}
string1 = "aferociousmonadatemyhamster"
string2 = "functionalprogrammingrules"

{- Ex3. Should return `1736` optimal alignments -}
--string1 = "bananrepubliksinvasionsarmestabsadjutant"
--string2 = "kontrabasfiolfodralmakarmästarlärling"

{- Print all optimal alignments between string1 and string2 -}
outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
  let optimals = optAlignmentsTable string1 string2
  mapM_ (\(x,y) -> mapM putStrLn [x,y,""]) optimals