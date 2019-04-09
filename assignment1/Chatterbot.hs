module Chatterbot where
import Utilities
import System.Random
import Data.Char

chatterbot :: String -> [(String, [String])] -> IO ()
chatterbot botName botRules = do
    putStrLn ("\n\nHi! I am " ++ botName ++ ". How are you?")
    botloop
  where
    brain = rulesCompile botRules
    botloop = do
      putStr "\n: "
      question <- getLine
      answer <- stateOfMind brain
      putStrLn (botName ++ ": " ++ (present . answer . prepare) question)
      if (not . endOfDialog) question then botloop else return ()

--------------------------------------------------------

type Phrase = [String]
type PhrasePair = (Phrase, Phrase)
type BotBrain = [(Phrase, [Phrase])]


--------------------------------------------------------

stateOfMind :: BotBrain -> IO (Phrase -> Phrase)
{- TO BE WRITTEN -}
-- stateOfMind _ = return id
stateOfMind brain = 
	do
		rand <- randomIO :: IO Float
		return (rulesApply((map.map2)(id, pick rand) brain))

rulesApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
-- rulesApply _ = id
ruleApply = try.TransformationsApply "*" reflect

reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
-- reflect = id
reflect = map reflectWord
	where reflectWord = try (flip lookup reflections)


reflections =
  [ ("am",     "are"),
    ("was",    "were"),
    ("i",      "you"),
    ("i'm",    "you are"),
    ("i'd",    "you would"),
    ("i've",   "you have"),
    ("i'll",   "you will"),
    ("my",     "your"),
    ("me",     "you"),
    ("are",    "am"),
    ("you're", "i am"),
    ("you've", "i have"),
    ("you'll", "i will"),
    ("your",   "my"),
    ("yours",  "mine"),
    ("you",    "me")
  ]


---------------------------------------------------------------------------------

endOfDialog :: String -> Bool
endOfDialog = (=="quit") . map toLower

present :: Phrase -> String
present = unwords

prepare :: String -> Phrase
prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") 

{- To Do: jonathanloganmoran -}
rulesCompile :: [(String, [String])] -> BotBrain
{- TO BE WRITTEN: Modified 9/4 by jalil-M -}
rulesCompile _ = []


--------------------------------------


reductions :: [PhrasePair]
reductions = (map.map2) (words, words)
  [ ( "please *", "*" ),
    ( "can you *", "*" ),
    ( "could you *", "*" ),
    ( "tell me if you are *", "are you *" ),
    ( "tell me who * is", "who is *" ),
    ( "tell me what * is", "what is *" ),
    ( "do you know who * is", "who is *" ),
    ( "do you know what * is", "what is *" ),
    ( "are you very *", "are you *" ),
    ( "i am very *", "i am *" ),
    ( "hi *", "hello *")
  ]

reduce :: Phrase -> Phrase
reduce = reductionsApply reductions


{- To-do 9/4: @jonathanloganmoran -}
reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply _ = id


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
{- TO BE WRITTEN: -}
--  MODIFIED: 9/4 by jonathanloganmoran
substitute _ [] _ = []
-- Cases to handle: 
---- Exists for x <-> s where both are present 
substitute f (x:xs) s
---- Does not exist for f: append to position and form returned list
    | f == x = s ++ substitute f xs s
---- Does not exist for x: tail recursion (cons)
    | otherwise = f : substitute f xs s

-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ _ _ = Nothing
{- TO BE WRITTEN -}
--  MODIFIED: 9/4 by jonathanloganmoran
match _ _ _ = Nothing
-- Cases to handle: 
--- Default case: wildcard matches
match _ [] [] = Just []
--- Second case: input is empty
match _ _ [] = Nothing
--- Third case: wildcard does not exist in sublist
match _ [] _ = Nothing

match wc (x:ps) (s:sl)
    | x == s = match wc ps sl
    | wc /= x = Nothing
    | otherwise longerWildcardMatch (x:ps) (s:sl) `orElse` singleWildcardMatch (x:ps) (s:sl)

{- MODIFIED 9/4: @jonathanloganmoran -}
-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
-- Cases to handle:
--- Expected case: first matches second
singleWildcardMatch [] [] = Just []
--- Second case: wildcard matches list
singleWildcardMatch _ [] = Nothing
--- Third case: list matches wildcard
singleWildcardMatch [] _ = Nothing
singleWildcardMatch (wc:ps) (x:xs) = mmap (const [x]) wc ps xs

{- TO BE WRITTEN -}
-- Cases to handle:
--- Expected case: wildcard retained, matches remaining list
longerWildcardMatch [] [] = Just []
--- Second case: wildcard removed, sublist matches after
longerWildcardMatch _ [] = Nothing
--- Third case: wildcard removed, sublist matches before
longerWildcardMatch [] _ = Nothing
longerWildcardMatch (wc:ps) (x:xs) = mmap (x :) (match wc (wc:ps) xs)


-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
--transformationApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}
transformationApply a f1 b (x, y) = mmap (substitute a y) (mmap f1 (match a x b))


-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
--transformationsApply _ _ _ _ = Nothing
{- TO BE WRITTEN -}
transformationsApply a f2 b list = foldl1 orElse (map (transformationApply a f2 list) b)


