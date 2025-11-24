import Data.List (sort, sortBy)
import Data.Ord (comparing)

-- minimum of a list of integers
-- we use pattern matching (and it is sensitive to order)
-- the list pattern [] matches on empty lists, [x] singleton lists, (x:xs) any non-empty list 
-- note: (x:xs) only is done after [x] and therefore only gets to match on non-singleton lists here
-- note: x:xs is a list of x's where x is the head element of the list and xs is the remaining tail of the list
mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

-- homemade min function
min' :: Int -> Int -> Int
min' x y 
    | x <= y    = x
    | otherwise = y

-- max of a list of Int
maxInt :: [Int] -> Int
maxInt [] = error "empty list"
maxInt [x] = x
maxInt (x:xs) = max x (maxInt xs)

-- removes first occurrence (Exercise 1.10)
removeFst :: Int -> [Int] -> [Int]
removeFst _ [] = [] -- if you pass anything and then an empty list, you get an empty list
removeFst m (x:xs) | m == x    = xs
removeFst m (x:xs) = x : removeFst m xs 

-- sort Ints (by removing the min and prepending to the front)
srtInts :: [Int] -> [Int]
srtInts [] = []
srtInts xs = m : srtInts (removeFst m xs) where m = mnmInt xs

-- alternative that doesn't use where construction
srtInts' :: [Int] -> [Int]
srtInts' [] = []
srtInts' xs = let
    m = mnmInt xs
    in m : srtInts' (removeFst m xs)

-- we use built in sum and length functions for lists
-- fromInegral converts Int to Float, so floating point division with / works
average :: [Int] -> Float
average [] = error "empty list"
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

-- homemade sum and length functions
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- notice there is no constraint on a's type, so there is no fat arrow => needed
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs


-- exercise 1.13 (count occurrences of chars in string)
-- uses pattern matching and guards
-- I didn't line up all of the output = signs. should i?
count :: Char -> String -> Int
count _ [] = 0
count m (x: xs) | m == x = 1 + count m xs
count m (x: xs) = count m xs

-- 1.14
blowupHelper :: Int -> String -> String
blowupHelper _ [] = []
blowupHelper i (x: xs) = replicate i x ++ blowupHelper (i+1) xs 

blowup :: String -> String
blowup = blowupHelper 1

-- you can just embed the helper function
blowup' :: String -> String
blowup' = blowupHelper' 1
  where
    blowupHelper' :: Int -> String -> String
    blowupHelper' _ [] = []
    blowupHelper' i (x: xs) = replicate i x ++ blowupHelper (i+1) xs

--1.15
-- The 'sort' function works directly on [String] because String (which is [Char]) has a built-in 'Ord' instance for alphabetical ordering.
srtString :: [String] -> [String]
srtString = sort

-- alternative: read in chars one at a time, left to right, and then use the srtInts :: [Int] -> [Int] or >, <, >= to sort
srtString' :: [String] -> [String]
srtString' = sortBy compareStrings

-- Manual comparison: compare strings lexicographically using recursion
compareStrings :: String -> String -> Ordering
compareStrings [] [] = EQ
compareStrings [] _  = LT
compareStrings _  [] = GT
compareStrings (x:xs) (y:ys)
  | x < y     = LT
  | x > y     = GT
  | otherwise = compareStrings xs ys

--1.16
prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x==y) && prefix xs ys

--1.17 (Note: we use just xs and not (x:xs) when we don't need the head or tail of the list )
substring :: String -> String -> Bool
substring [] ys = True
substring xs [] = False
substring xs (y:ys) = prefix xs (y:ys) || substring xs ys

