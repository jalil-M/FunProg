module Parser(module CoreParser, T, digit, digitVal, chars, letter, err, line_in,
              lit, number, iter, accept, require, token,
              spaces, word, (-#), (#-)) where
import Prelude hiding (return, fail)
import Data.Char
import CoreParser
infixl 7 -#, #-

type T a = Parser a

err :: String -> Parser a
err message cs = error (message++" near "++cs++"\n")

iter :: Parser a -> Parser [a]
iter m = m # iter m >-> cons ! return []

cons(a, b) = a:b

{- BEGIN Task 1: Parser functions implementation -}

(-#) :: Parser a -> Parser b -> Parser b
-- accepts the same input as `m # n` but returns just the result from `n` parser
-- declared as a left associative infix operator with precedence 7
m -# n = m # n >-> snd

(#-) :: Parser a -> Parser b -> Parser a
--accepts the same input as `m # n` but returns the result from the `m` parser
m #- n = m # n >-> fst

token :: Parser a -> Parser a
token m = m #- spaces

letter :: Parser Char
-- parser for a letter as defined by the Prelude `isAlpha` function
letter = char ? isAlpha

spaces :: Parser String
-- accepts any number of whitespace characters
spaces = iter (char ? isSpace)

chars :: Int -> Parser String
chars 0 = return []
-- accepts n number of characters
chars n = char # chars (n-1) >-> cons

word :: Parser String
word = token (letter # iter letter >-> cons)

accept :: String -> Parser String
accept w = (token (chars (length w))) ? (==w)

require :: String -> Parser String
-- reports the missing string using `err` in case of failure
require wrd = accept wrd ! err wrd

lit :: Char -> Parser Char
lit c = token char ? (==c)

digit :: Parser Char
digit = char ? isDigit

digitVal :: Parser Integer
digitVal = digit >-> digitToInt >-> fromIntegral

number' :: Integer -> Parser Integer
number' n = digitVal #> (\ d -> number' (10*n+d))
          ! return n
number :: Parser Integer
number = token (digitVal #> number')

line_in :: Parser String
line_in = iter (char ? (/='\n'))
