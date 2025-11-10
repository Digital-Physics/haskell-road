-- minimum of a list of integers
-- we use pattern matching (and it is sensitive to order)
-- the list pattern [] matches on empty lists, [x] singleton lists, (x:xs) any non-empty list 
-- note: (x:xs) only is done after [x] and therefore only gets to match on non-singleton lists here
-- note: x:xs is a list of x's where x is the head element of the list and xs is the remaining tail of the list
mnmInt :: [Int] -> Int
mnmInt [] = error "empty list"
mnmInt [x] = x
mnmInt (x:xs) = min x (mnmInt xs)

-- home-made min function
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
    