-- {-# LANGUAGE InstanceSigs #-}
module Chapter4 where

import qualified Data.Set as Set --qualified removes ambiguity and forces explicit calls of functions with same names as others in scope
import Data.List (delete) -- selective import of delete, which deletes first occurrence in list


naturals :: [Integer]
naturals = [0..]

small_squares2 :: [Integer]
small_squares2 = [n^2 | n <- naturals, n < 10]

-- Halting prob example: Collatz conjecture; termination is difficult to prove
run :: Integer -> [Integer]
run n | n < 1     = error "argument not positive"
      | n == 1    = [1]
      | even n    = n : run (div n 2)
      | odd n     = n : run (3 * n + 1)
      | otherwise = error "the linting said I didn't cover all possibilities?"

-- *Chapter4> run 10
-- [10,5,16,8,4,2,1]
-- *Chapter4> run 5
-- [5,16,8,4,2,1]

-- *Chapter4> elem "R" "Russell"
-- <interactive>:11:10: error:
--     • Couldn't match type ‘Char’ with ‘[Char]’
--       Expected type: [[Char]]
--         Actual type: [Char]
--     • In the second argument of ‘elem’, namely ‘"Russell"’
--       In the expression: elem "R" "Russell"
--       In an equation for ‘it’: it = elem "R" "Russell"
-- *Chapter4> elem 'R' "Russell"
-- True

-- Type judgement shows that this infix operator takes in an item of type a then a list of type a and returns the concatenation
-- Prelude> :t (:)
-- (:) :: a -> [a] -> [a]

-- 4.47
-- Implementation of `nub` (removing duplicates)
-- What this says is, first, that if a is any type for which a relation of equality is defined, 
-- then nub operates on a list over type a and returns a list over type a.
nub' :: (Eq a) => [a] -> [a]
nub' [] = []
nub' (x:xs) = x : nub' (remove x xs)
  where
    remove y [] = []
    remove y (z:zs) | y == z    = remove y zs
                    | otherwise = z : remove y zs

-- *Chapter4> nub' "Mississippi"
-- "Misp"

-- split a list into all possible non-empty two-part combinations
splitList :: [a] -> [([a],[a])]
splitList xs = [splitAt n xs | n <- [1..(length xs - 1)]]

-- *Chapter4> splitAt 3 [1..10]
-- ([1,2,3],[4,5,6,7,8,9,10])
-- *Chapter4> splitList[1..4]
-- [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]


-- Section 4.7: List Comprehension and Database Query
-- A small relational database implemented using lists of strings, 

type WordList = [String]
type DB = [WordList]

db :: DB
db = [
  ["release", "Blade Runner", "1982"],
  ["release", "Alien", "1979"],
  ["direct", "Ridley Scott", "Alien"],
  ["direct", "Ridley Scott", "Blade Runner"],
  ["play", "Harrison Ford", "Blade Runner", "Rick Deckard"],
  ["play", "Sigourney Weaver", "Alien", "Ellen Ripley"],
  ["play", "Ridley Scott", "Fake Movie", "Fake Character"] -- fake record to see a non-empty q1
  ]

-- Helper functions to extract entities
directors :: [String]
directors = nub' [ x | ["direct",x,_] <- db ]

-- *Chapter4> directors
-- ["Ridley Scott"]

actors :: [String]
actors = nub' [ x | ["play",x,_,_] <- db ]

-- *Chapter4> actors
-- ["Harrison Ford","Sigourney Weaver"]

-- Predicate to check if someone is a director
directorP :: String -> Bool
-- directorP = \ x -> elem x directors
-- directorP x = elem x directors
directorP x = x `elem` directors

-- *Chapter4> directorP "Ridley Scott"
-- True

-- Query 1: Give me the actors that are also directors
q1 :: [String]
q1 = [ x | x <- actors, directorP x ]

-- *Chapter4> q1
-- ["Ridley Scott"]

-- Query 2: Give me all actors that are also directors, with the films they acted in
q2 :: [(String, String)]
q2 = [ (x,y) | (x,y) <- act, directorP x ]
       where act = [ (x,y) | ["play",x,y,_] <- db ]

-- *Chapter4> q2
-- [("Ridley Scott","Fake Movie")]

-- Section 4.8: Using Lists to Represent Sets
-- Set theoretic operations implemented on standard lists. 
-- These assume no duplicates are present in the inputs.

-- Set Union using lists
union' :: Eq a => [a] -> [a] -> [a]
union' [] ys = ys
union' (x:xs) ys = x : union' xs (delete x ys)

-- Set Intersection using lists
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] s = []
intersect' (x:xs) s | elem x s  = x : intersect' xs s
                   | otherwise = intersect' xs s

-- Power Set: generating all sublists
powerList :: [a] -> [[a]]
powerList [] = [[]]
powerList (x:xs) = (powerList xs) ++ (map (x:) (powerList xs))

-- *Chapter4> powerList "bob"
-- ["","b","o","ob","b","bb","bo","bob"]

-- 4.9: A Data Type for Sets
-- This line of Haskell code defines a wrapper type. It creates a custom type named Set that is internally 
-- represented by a list ([a]), but is treated as a distinct type by the compiler.
-- deriving (Show) automatically generates the code required to convert this type to a String, to print set to GHCi console
newtype Set a = Set [a] deriving (Show)

-- Equality instance for Set: ignores order of elements
instance Eq a => Eq (Set a) where
  -- (==) :: Eq a => Set a -> Set a -> Bool  -- can't do this type signature without InstanceSigs
  set1 == set2 = subSet set1 set2 && subSet set2 set1

-- Create an empty set
emptySet :: Set a
emptySet = Set []

-- Check if set is empty
isEmpty :: Set a -> Bool
isEmpty (Set []) = True
isEmpty _        = False

-- Check membership
inSet :: (Eq a) => a -> Set a -> Bool
inSet x (Set s) = elem x s

-- Check for subset relationship
subSet :: (Eq a) => Set a -> Set a -> Bool
subSet (Set []) _ = True
subSet (Set (x:xs)) set = (inSet x set) && subSet (Set xs) set

-- Insert an element into a set (maintaining uniqueness)
insertSet :: (Eq a) => a -> Set a -> Set a
insertSet x (Set ys) | inSet x (Set ys) = Set ys
                     | otherwise        = Set (x:ys)

--Convert a list to a set
list2set :: Eq a => [a] -> Set a
list2set [] = Set []
list2set (x:xs) = insertSet x (list2set xs)

-- *Chapter4> list2set [1, 3, 2, 3]
-- Set [1,2,3]

--4.54

unionSet :: (Eq a) => Set a -> Set a -> Set a
unionSet (Set []) set2 = set2
unionSet (Set (x:xs)) set2 = insertSet x (unionSet (Set xs) set2)

intersectSet :: (Eq a) => Set a -> Set a -> Set a
intersectSet (Set []) set2 = emptySet
intersectSet (Set (x:xs)) set2 
  | inSet x set2 = insertSet x (intersectSet (Set xs) set2)
  | otherwise    = intersectSet (Set xs) set2

differenceSet :: (Eq a) => Set a -> Set a -> Set a
differenceSet (Set []) set2 = emptySet
differenceSet (Set (x:xs)) set2
  | inSet x set2 = differenceSet (Set xs) set2
  | otherwise    = insertSet x (differenceSet (Set xs) set2)

-- this book is old; don't roll your own Set
-- import qualified Data.Set as Set which implements Balance Binary trees
--Data.set

mySet :: Set.Set Integer
mySet = Set.fromList [5, 1, 5, 2, 1, 3]

hasTwo :: Bool
hasTwo = Set.member 2 mySet

largerSet :: Set.Set Integer
largerSet = Set.insert 99 mySet

smallerSet :: Set.Set Integer
smallerSet = Set.delete 1 largerSet

-- *Chapter4> mySet
-- fromList [1,2,3,5]
-- *Chapter4> hasTwo
-- True
-- *Chapter4> largerSet
-- fromList [1,2,3,5,99]
-- *Chapter4> smallerSet
-- fromList [2,3,5,99]