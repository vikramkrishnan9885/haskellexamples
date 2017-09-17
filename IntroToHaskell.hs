{- ghci command
    Start ghci:
    shell-prompt>ghci                   -- or 
    shell-prompt>ghci IntroToHaskell.hs
    
    Prelude>:load IntroToHaskell.hs     -- load the definitions in IntroToHaskell.hs
    Prelude>:r                          -- reload the previous file.
    
    Prelude>:set +t                     -- asks to display type information.
    Prelude>:unset +t                   -- asks not to display type information.
    Prelude>:browse                     -- display list of current bindings

There are two comment forms in Haskell: 
    {- comment -} and 
    --comment to the end of the line
-}

module Main where
import Data.List
import Test.QuickCheck

ls = [1,2,3]
rs = foldl (\accumulator item -> item + accumulator) 0 ls








{-  We can type expressions directly into ghci, 
    which provides a read-eval-print loop:

(5+3) - 2
if 5>3 then "Harry" else "Hermione"
5 == 4

ghci infers types before compiling or executing.  
ghci looks like an interepter, but is really compiling the code snippets.


-}

















-- Overview by Type
-- Base Types
-- Bool
true  = True
false = False

-- As this example shows, in Haskell, expression variables must start with a 
-- lower case letter, while data constructors (and type constructors)
-- must start with a capital letter.





-- Types of the two branches of conditional must be the same
ifExpression = if true then 10 else 13

{-  For example, with a mismatch, we get an error like the following:
*Main> if true then "a" else 1

<interactive>:25:23:
    No instance for (Num [Char]) arising from the literal ‘1’
    In the expression: 1
    In the expression: if true then "a" else 1
    In an equation for ‘it’: it = if true then "a" else 1
-}









-- BASE TYPES
-- Integers
anInt = 2
anIntExpression = (2 * 5 - 2) `mod` 2


-- Strings (note, String = [Char])
aString :: String           --  "::" means "has type"
aString = "Ron Weasley"
bString = "Severus Snape"


-- Real numbers
e = 2.714
aFloat :: Float
aFloat = 2



-- Simple compound types
-- Tuples
aTuple = (4, "Griffendor")

-- Functions to deconstruct a pair:
-- fst :: (a, b) -> a
-- snd :: (a, b) -> b

newTuple = (fst aTuple, snd aTuple)









-- Lists
nilList :: [a]             -- Polymorphic type: Lowercase type identifiers are type variables.
nilList = []               

anIntList = 1 : [2,3,4]    -- Infix cons notation (:)   
                           -- Note, ML uses :: for cons and : for "has type".

-- Functions to deconstruct a list:
-- head :: [a] -> a
-- tail :: [a] -> [a]







-- Records
data Person = Person {firstName :: String, lastName :: String}
  deriving Show        -- Tells ghci to automatically generate code to print Person records

hg = Person {firstName = "Hermione", lastName = "Granger"}
fname = firstName hg









-- Patterns and Declarations
-- Patterns can be used in place of variables
-- <pat> ::= <var> | <tuple> | <cons> | <record> | ...
--
-- Value declarations
-- <pat> = <exp>

myTuple = ("Flitwick", "Snape")
(x,y) = myTuple

myList = [1,2,3,4]
z:zs = myList



-- Let allows us to introduce local declarations
localDecl = let (x,y) = (2,"Snape") in x * 4 




-- In 
--   let <pat> = <exp1> in <exp2>
-- the variables bound in <pat> are in scope in both <exp1> and <exp2>

myinflist = let p = 1 : p in 2 : p



-- Functions and Pattern Matching
anonymousFunction = \x -> x + 1      -- Like Lisp lambda, function (...) in Javascript


-- Function declaration form:
-- <name> <pat1> = <exp1>
-- <name> <pat2> = <exp2>
-- <name> <pat3> = <exp3>


-- Single-branch function defined using tuple pattern as argument:
f (x,y) = x + y

-- Curried version of f:
g x y = x + y


-- Multiple-branch function defined using list pattern as argument.
myLength [] = 0
myLength (x:xs) = 1 + myLength xs








-- Map function on Lists
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs


applyMap = myMap (\x -> x + 1) [1,2,3]


-- or equivalently,
applyMap2 = myMap (+ 1) [1,2,3]



-- More functions on lists.
append([], ys) = ys
append(x:xs, ys) = x : append(xs, ys)

-- Built-in operator (++) appends any two lists of the same type.

myReverse [] = []
myReverse (x:xs) =  (reverse xs) ++ [x] 

-- How efficient is reverse?










-- This pattern of writing a related function with an extra "accumulator" 
-- is very common in functional programming.
accumReverse xs =
    let rev ( [], accum ) = accum
        rev ( y:ys, accum ) = rev( ys, y:accum )
    in rev( xs, [] )





-- List Comprehensions
myData = [1,2,3,4,5,6,7]
twiceData = [ 2 * x | x <- myData]

-- with a predicate
twiceEvenData = [ 2 * x | x <- myData, x `mod` 2 == 0]






-- Datatype declarations
-- Type Names and data constructors start with capital letters.
data Color = Red | Yellow | Blue        -- Elements of type Color are Red, Yellow, and Blue
   deriving Show

-- General form for datatype declarations:
-- data <Name> = <clause> | ... | <clause>                 
-- <clause> ::=  <Constructor> <type> ... <type>     

-- Note: type list can be empty, as in Color example


-- Data constructors can take arguments
data Atom = Atom String | Number Int   
 deriving Show
atom1 = Atom "oxygen"
atom2 = Number 8


-- Data declarations can be recursive.
data AtomList = Nil | Cons  (Atom, AtomList)
  deriving Show
list1 = Nil
list2 = Cons (atom1, list1)







-- Data declarations can be parameterized by type variables
data Tree a = Leaf a | Node (a, Tree a, Tree a)
            deriving Show

aTree = Node(4, Node(3, Leaf 1, Leaf 2),
                Node(5, Leaf 6, Leaf 7))         


-- Functions over datatypes use pattern matching over data constructors to access values:
tsum (Leaf n) = n
tsum (Node (n,t1,t2)) = n + tsum t1 + tsum t2

-- Example: Evaluating Expressions
-- Define datatype of expressions:
data Exp = Var String | Const Int | Plus (Exp, Exp)
           deriving Show

-- exampleExp is (x + 3) + y
exampleExp =  Plus(Plus(Var "x", Const 3), Var "y")

-- We can also use a case expression to deconstruct values of a datatype:
-- Indentation matters in branches of a case in Haskell.  
-- All branches must start at the same column.
exampleCase = case exampleExp of
               Var n ->  0
               Const n -> 1
               Plus(e1,e2) -> 2 





-- Suppose we want to write a function to simplify our expressions
exampleExp2 = Plus(Const 3, Const 2)                 -- Const 5
exampleExp3 = Plus(Var "x", Plus(Const 2, Const 3))  -- Plus (Var "x", Const 5)


-- Definition of simplify function:
simplify ( Var s) = Var s
simplify ( Const n ) = Const n
simplify ( Plus ( e1,e2 ) ) = 
     case simplify e1 of
          Var s -> Plus( Var s, simplify e2)      
          Const n -> case simplify e2 of  
                              Var s -> Plus(Const n, Var s)      
                              Const m -> Const (n+m)                         
                              Plus(e3,e4) -> Plus ( Const n, Plus ( e3, e4 ))   
          Plus(e3, e4) -> Plus( Plus ( e3, e4 ), simplify e2)





-- Laziness
-- Haskell is lazy language.
-- Functions and data constructors don't evaulate their arguments until they need them.


-- Programmers can write their own control-flow operators:
cond :: Bool -> a -> a -> a 
cond True  t e = t
cond False t e = e


-- Why can't programmers write such operations in eager languages?


-- Using laziness
-- "Harry" `isSubString` "Harry Potter"
-- (Putting ticks around a function makes it an infix operator.)

isSubString :: String -> String -> Bool
x `isSubString` s = or [ x `isPrefixOf` t | t <- suffixes s ]

-- All suffixes of s
suffixes :: String -> [String]
suffixes []     = [""]
suffixes (x:xs) = (x:xs) : suffixes xs



-- Another example of laziness
-- (myOr bs) returns True if any of the bs is True
myOr :: [Bool] -> Bool
myOr []     = False
myOr (b:bs) = b || myOr bs



-- Infinite data structures
-- Laziness allows us to use conceptually infinte data structures.  Because values
-- are only computed when they are needed, the infinite structure is not materialized
-- (Unless the code is looping infinitely trying to produce the next value.)

-- The haskell notation [n..] generates the infinite sequence of natural numbers starting at n

naturals = [0..]

-- The function 'take n list' returns the first n elements of the list 'list', so 
-- first10 is the list [0,1,2,3,4,5,6,7,8,9]
first10 = take 10 naturals


-- primesN is the infinite list of primes.
-- It is defined using list comprehension notation.
primesN :: [Int]
primesN = 
   let
    sieve(p:xs) = p : sieve [ x | x <- xs, x `mod` p > 0]
   in sieve [2..]


-- A lazy paradigm
-- Generate all solutions (a potentially enormous tree)
-- Walk the tree to find the solution you want

nextMove :: Board -> Move
nextMove b = selectMove allMoves
  where allMoves = allMovesFrom b

data Board = Board  -- Put real definition here
data Move = Move    -- Put real definition here
selectMove moves = undefined   -- undefined is halts with an error, has polymorphic type a
allMovesFrom b =   undefined   -- replace with real definition



-- Testing
-- It is good to write tests as you write code.
-- Printf-style debugging doesn't work well in Haskell.  Why?


-- You can use ghci as an interactive shell to test code as you write it,
-- which turns out to be quite a powerful debugging feature.



-- The QuickCheck library helps with testing by generating random test data
-- automatically from the type of the function being tested.
-- Consider the function 'eReverse' that reverses a list using an accumulator:
eReverse xs =
    let rev ( [], z ) = []
        rev ( y:ys, z ) = rev( ys, y:z )
    in rev( xs, [] )

-- Define a non-polymorphic type at which to test the function:
type TS =  [Int] 

-- A property is a function from the testing type to Bool
-- Define property we wish to test:
prop_RevRev :: TS -> Bool
prop_RevRev ls = eReverse (eReverse ls) == ls

-- Run in ghci
test_result= quickCheck prop_RevRev

-- We can fix the function and rerun property checker.
-- Note that quickCheck is a Haskell ** library **.  There is no special interaction
-- with ghc or ghci
 
-- This file can also be compiled, 
--   using ghc --make IntroToHaskell.hs
main =  
  putStrLn "Code snippets from lecture on an Introduction to Haskell. \nUse ghci IntroToHaskell.hs to play with snippets."