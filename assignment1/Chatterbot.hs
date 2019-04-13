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
stateOfMind brain =
  do
    rand <- randomIO :: IO Float
    return (rulesApply((map . map2)(id, pick rand) brain))

rulesApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
rulesApply = try . (transformationsApply "*" reflect)

reflect :: Phrase -> Phrase
{- TO BE WRITTEN -}
reflect = (map . try)(flip lookup reflections)

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

rulesCompile :: [(String, [String])] -> BotBrain
{- TO BE WRITTEN -}
rulesCompile = (map.map2) (f, map f)
  where f = words . map toLower


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

reductionsApply :: [PhrasePair] -> Phrase -> Phrase
{- TO BE WRITTEN -}
reductionsApply = fix . try . transformationsApply "*" id

-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
{- TO BE WRITTEN -}
substitute _ [] _ = []
substitute w (x:xs) y
    | x == w = y ++ (substitute w xs y)
    | otherwise = x : (substitute w xs y)


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
{- TO BE WRITTEN -}
match _ [] [] = Just []
match _ _ [] = Nothing
match _ [] _ = Nothing
--match w p s
--    | head p == w = orElse (singleWildcardMatch p s) (longerWildcardMatch p s)
--    | head p == head s = match w (tail p) (tail s)
--    | otherwise = Nothing
match wc (x:ps) (s:sl)
  | x == s = match wc ps sl
  | wc /= x = Nothing
  | otherwise = longerWildcardMatch (x:ps) (s:sl) `orElse` singleWildcardMatch (x:ps) (s:sl)


-- Helper function to match
singleWildcardMatch, longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
--singleWildcardMatch (w:xs) (p:ps) = mmap (const [p]) (match w xs ps)
--longerWildcardMatch (w:xs) (p:ps) = mmap (p:) (match w (w:xs) ps)
singleWildcardMatch (wc : ps) (x : xs) = mmap (const [x]) (match wc ps xs)
longerWildcardMatch (wc : ps) (x : xs) = mmap (x :) (match wc (wc:ps) xs)
--singleWildcardMatch [] [] = Just []
--singleWildcardMatch _ [] = Nothing
--singleWildcardMatch [] _ = Nothing
--longerWildcardMatch [] [] = Just []
--longerWildcardMatch _ [] = Nothing
--longerWildcardMatch [] _ = Nothing


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
transformationApply a f1 b (x, y) = mmap (substitute a y) (mmap f1 (match a x b))
--transformationApply wildcard func target (key, value) = mmap (substitute wildcard value) (mmap func (match wildcard key target))
--transformationApply w f list pattern = mmap (substitute w (snd pattern) . f) (match w (fst pattern) list)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply a f2 b list = foldl1 orElse (map (transformationApply a f2 list) b)
--transformationsApply wildcard fun dictionary lookupList = foldl1 orElse (map (transformationApply wildcard fun lookupList) dictionary)
--transformationsApply _ _ [] _ = Nothing
--transformationsApply w f (p:ps) list = orElse (transformationApply w f list p) (transformationsApply w f ps list)
