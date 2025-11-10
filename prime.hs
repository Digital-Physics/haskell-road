-- rem is a built-in remainder function
-- Integral is an integer type (i.e. Int or Integer) and not a Float or Double
-- "For any type a that satisfies the constraint of being an instance of the Integral class, the function takes an a, then a String, and finally returns a Bool."
-- if we were fine with just integer types, our type declaration could just be "divides :: Integer -> Integer -> Bool"
divides :: Integral a => a -> a -> Bool
divides d n = rem n d == 0

-- least divisor of n, LD(n) 
-- is the same as least divisor starting From a threshold k if k = 2, LDF(k)(n) -- I'm getting a suggestion to change the function to ld = ldf 2, which I think makes sense
ld :: Integral t => t -> t
-- ld n = ldf 2 n
ld = ldf 2

-- "equation guarding", is like a if{}-else-if{}...-else{}
--  the expression condition to the lef of the = is the guard
-- shorthand for
-- foo t | condition_i = body_i
-- foo t | condition_i = body_i
-- foo t | condition_i = body_i
-- foo t               = body_n
ldf :: Integral t => t -> t -> t
ldf k n | divides k n   = k
        | k^2 > n       = n
        | otherwise     = ldf (k+1) n

prime0 :: Integral a => a -> Bool
prime0 n
    | n < 1     = error "not a positive integer"
    | n == 1    = False
    | otherwise = ld n == n
        
