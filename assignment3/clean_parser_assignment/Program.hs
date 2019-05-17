module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)

{- Task 5. Represent the program as a `Statement` list -}
newtype T = Program [Statement.T] deriving Show

{- Task 5. Define the parse function in this module -}
instance Parse T where
  parse = (iter Statement.parse) >-> Program
  	--error "Program.parse not implemented"
  toString (Program a) = foldr1 (++) (map Statement.toString a) 
  	--error "Program.toString not implemented"

{- Task 5. Use the `exec` function to execute a program -}
exec :: T -> [Integer] -> [Integer]
	--error "Program.exec not implemented"
-- Hint 3d: Make a recursive call with a new dictionary
exec (Program a) b = Statement.exec a Dictionary.empty b