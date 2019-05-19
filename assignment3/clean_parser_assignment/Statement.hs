module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T
        | If Expr.T Statement Statement
        | While Expr.T Statement
        | Read String
        | Write Expr.T
        | Skip
        | Begin [Statement]
        | Comment String
    deriving Show

{- Background info: Parser operators (Andersson 2001)
 - (>->) transforms the result of a parser, returns Int from digit
 - (-#)  applies two parsers in sequence but discards first result
 - (#-)  applies two parsers in sequence but discards second result
-}

{- Task 3a. Write constructors for the seven kinds of statements -}
assignment, ifStmt, while, readStmt, write, skip, begin, comment :: Parser Statement

{- Task 3b: Define a parsing function for each kind of statement -}
assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

ifStmt = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((a,b), c) = If a b c

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (v, e) = While v e

readStmt = accept "read" -# word #- require ";" >-> buildRead
buildRead b = Read b

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite c = Write c

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip _ = Skip

begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin a = Begin a

{- Task 5. Add `comment` functionality to parser -}
comment = accept "--" -# line_in #- require "\n" >-> buildComment
buildComment = Comment

{- Task 3d. Write the exec functions for each kind of statement -}
exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

-- Hint 3d: `exec` takes a list of stmts to be executed, a dict tuple, and the ints to be `read`
-- Hint 3d: `exec` returns a list of numbers produced by `write` statements
exec [] _ _ = []

-- Hint 3d: `assignment` makes a recurisve call with a new dictionary
exec (Assignment name expr: stmts) dict input = exec stmts (Dictionary.insert(name, Expr.value expr dict) dict) input

-- Hint 3d: example execution of a conditional statement
exec (If expr thenStmts elseStmts: stmts) dict input =
    if (Expr.value expr dict) > 0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (While expr stmt: stmts) dict input =
  if (Expr.value expr dict) > 0
  then exec (stmt:(While expr stmt: stmts)) dict input
  else exec stmts dict input

-- Hint 3d: `write` adds value to the returned list
exec (Write expr : stmts) dict input =  Expr.value expr dict : exec stmts dict input

exec (Read expr : stmts) dict (it:input) = exec stmts (Dictionary.insert (expr, it) dict) input

exec (Skip : stmts) dict input = exec stmts dict input

exec (Begin list : stmts) dict input = exec (list ++ stmts) dict input

exec (Comment ln : stmts) dict input = exec stmts dict input

toString' :: Statement -> String
toString' (Assignment name expr) = name ++ " := " ++ Expr.toString expr ++ ";\n"
toString' (If cond thenStmt elseStmt) = "if " ++ Expr.toString cond ++ "\nthen\n" ++ toString thenStmt ++ "else\n" ++ toString elseStmt
toString' (While expr stmts) = "while " ++ Expr.toString expr ++ " do\n" ++ toString stmts
toString' (Read name) = "Read " ++ name ++ ";\n"
toString' (Write expr) = "Write " ++ Expr.toString expr ++ ";\n"
toString' (Skip) = "skip;\n"
toString' (Begin list) = "Begin\n" ++ foldr1 (++) (map toString list) ++ "end\n"
toString' (Comment ln) = "-- " ++ ln ++ ";\n"

{- Task 3c: Use these functions to define `parse` -}
instance Parse Statement where
  parse = assignment ! ifStmt ! while ! readStmt ! write ! skip ! begin ! comment
  toString = toString'
