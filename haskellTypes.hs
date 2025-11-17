-- 1.6
-- basic Haskell types

-- 1. Numeric Types (also see numbers.hs)
-- Int: Fixed-precision integer (standard size, fast).
exampleInt :: Int
exampleInt = 42

-- Integer: Arbitrary-precision integer (can be very large, slower).
exampleInteger :: Integer
exampleInteger = 100000000000000000000000000000000000000000000

-- Float: Single-precision floating-point number.
exampleFloat :: Float
exampleFloat = 3.14159

-- Double: Double-precision floating-point number (most common for real numbers).
exampleDouble :: Double
exampleDouble = 2.718281828459

-- 2. Character and String Types
-- Char: A single character, enclosed in single quotes.
exampleChar :: Char
exampleChar = 'z'

-- String: A list of characters. Note: String is a type synonym for [Char].
exampleString :: String
exampleString = "Haskell is cool"

-- 3. Boolean and Unit Types
-- Bool: Boolean values (True or False).
exampleBool :: Bool
exampleBool = True

-- (): The Unit type, which has only one possible value, ().
-- Often used when a function needs to return "nothing" of importance.
exampleUnit :: ()
exampleUnit = ()

-- 4. Constructed Type Examples (Review)
-- List: Homogeneous collection [a]
exampleList :: [Int]
exampleList = [1, 5, 9, 13]

-- Tuple: Fixed-size, heterogeneous collection (a, b, ...)
exampleTuple :: (String, Double, Bool)
exampleTuple = ("pi approx", 3.14, True)

-- Type: () (Unit)
-- The Unit type has only one value, '()'. Often used as a placeholder.
unitExample :: ()
unitExample = ()

-- Type: Int vs Integer
-- Int: Fixed-size integer (usually 32 or 64 bits). Fast, but can overflow.
intExample :: Int
intExample = 123

-- Integer: Arbitrarily large integer. Slower, but never overflows.
integerExample :: Integer
integerExample = 99999999999999999999999999999999999999999999999999

-- type variable are a, b, etc.

-- new types 
-- list formations (lists are homogenous; all items must be same type)
exampleList1 :: [Int]
exampleList1 = [1, 2, 3, 4]

exampleList2 :: [[Char]] -- Same as [String]
exampleList2 = ["hello", "world"]

-- pair and tuple formations
exampleTuple1 :: (Int, String)
exampleTuple1 = (42, "meaning")

exampleTuple2 :: (Bool, Char, [Int])
exampleTuple2 = (True, 'c', [1, 2, 3])

-- function definitions
-- a function from a -> b is a type
exampleFunction1 :: Int -> Int
exampleFunction1 x = x * 2

exampleFunction2 :: [a] -> Int
exampleFunction2 xs = length xs -- Polymorphic: works for any list of a's [a]

-- creating your own types
exampleListTuple :: [(Int, Bool)]
exampleListTuple = [(1, True), (2, False)]

-- Currying (associates to the right)
-- two steps; returns a transformer function after first string is passed
stringfunction :: String -> (String -> String)
stringfunction _ _ = error "just a demo"

-- is the same as
stringfunction' :: String -> String -> String
stringfunction' _ _ = error "just a demo"

-- Example: The (+) operator's type:
-- (+) :: Int -> (Int -> Int)  -- Takes an Int, returns a function that takes an Int and returns an Int.

addFive :: Int -> Int
addFive = (+) 5 -- Partial application: 'addFive' is the result of passing '5' to (+)

--1.18
expression1 :: [String]
expression1 = ["cheese", "whiz"]

expression2 :: (Bool, String)
expression2 = (True, "whiz wid")

expression3 :: [(Bool, String)]
expression3 = [(True, "whiz wid"), (False, "provolone") ]

expression4 :: ([Bool], String)
expression4 = ([True, True, True, True, True, True, True, True, True, True, True, True, True, True, True, False], "NYRB")

expression5 :: Bool -> Bool
expression5 True = True
expression5 False = True

--1.19 predefined function types (with :t <function name>)
-- These functions illustrate the use of type variables (polymorphism).

-- head :: [a] -> a -- first item in list

-- last :: [a] -> a -- last item in list

-- init :: [a] -> [a] -- everything but last element

-- fst :: (a, b) -> a -- first item from tuple

-- (++) :: [a] -> [a] -> [a] -- concatenate lists
-- *Main> (++) [3] [1, 2]
-- [3,1,2]
-- *Main> (++) [3] [1.0, 2.0]
-- [3.0,1.0,2.0]
-- *Main> (++) ["alice"] [1, 2]
-- <interactive>:46:17: error:
--     • No instance for (Num [Char]) arising from the literal ‘1’
--     • In the expression: 1
--       In the second argument of ‘(++)’, namely ‘[1, 2]’
--       In the expression: (++) ["alice"] [1, 2]

-- flip :: (a -> b -> c) -> b -> a -> c

-- flip is a higher-order function that takes a function (a -> b -> c) and returns a new function (b -> a -> c)
-- that takes the arguments in the opposite order. so let's look at an operator that is non-commutative, where the order matters.
standardSubtract :: Num a => a -> a -> a
standardSubtract x y = y - x

-- Applying 'flip' to 'subtract'
flippedSubtract :: Num a => a -> a -> a
flippedSubtract x y = x - y

flippedSubtract' :: Num a => a -> a -> a
flippedSubtract' = flip standardSubtract

-- flip is very useful when you want to partially apply a function but need to fix the second argument instead of the first (which is the default behavior due to currying).

flipConcatOrder :: [a] -> [a] -> [a]
flipConcatOrder = flip (++)
-- This is a partial application of 'flip' to '(++)', but the type remains the same because (++) already has a symmetric type signature:
-- (++) :: [a] -> [a] -> [a]
-- flip (++) :: [a] -> [a] -> [a] -- The arguments are of the same type, so the reversal isn't visible in the type.
