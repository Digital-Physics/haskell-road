-- 1.8
-- list abbreviations
-- [4..20] is [4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
-- ['a'..'z'] is "abcdefghijklmnopqrstuvwxyz"

-- map takes a function and a list and applies the function to each element in the list
-- Prelude> :t map
-- map :: (a -> b) -> [a] -> [b]

-- If f is a function of type a -> b and xs is a list of type [a], then map f xs will
-- return a list of type [b]. E.g., map (^2) [1..9] will produce the list of squares
-- [1, 4, 9, 16, 25, 36, 49, 64, 81]
square :: Int -> Int
square n = n * n

-- Conversion of Infix to Prefix for the operator ^
-- built-in function (^2) represents the "construction of sections"
-- map (^2) [1..9]
-- map square [1..9]

greaterThanThree :: Integral a => a -> Bool
greaterThanThree = (>3)
-- map (>3) [1..9]
-- map greaterThanThree [1..9]

-- homemade map
map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map f xs

-- 1.20
-- note: the lists have to be lists of the same type
lengths :: [[a]] -> [Int]
lengths = map length

-- 1.21
sumLengths :: [[a]] -> Int
sumLengths xs = sum (map length xs)

-- uses the composition operator .
sumLengths' :: [[a]] -> Int
sumLengths' = sum . map length

-- fails; The sum function requires a list of numbers as its argument, not a function.
-- sumLengths'' :: [[a]] -> Int
-- sumLengths'' = sum (map length)

-- Filter

-- homemade filter
filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
-- if the filter function returns True on the first item, add it to the output and process the rest of the list
filter' p (x:xs)    | p x       = x : filter' p xs 
-- if the filter function returns False skip it and process the rest of the list
                    | otherwise = filter' p xs

-- 1.22 & 1.23 (see prime.hs)
-- primes0 :: [Integer]
-- primes0 = filter prime0 [2..]

