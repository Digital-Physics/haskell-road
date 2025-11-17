-- rem is a built-in remainder function
-- Integral is an integer type (i.e. Int or Integer) and not a Float or Double
-- "For any type a that satisfies the constraint of being an instance of the Integral class, the function takes an a, then a String, and finally returns a Bool."
-- if we were fine with just integer types, our type declaration could just be "divides :: Integer -> Integer -> Bool" which is short for Integer -> (Integer -> Bool).
divides :: Integral a => a -> a -> Bool
divides d n = rem n d == 0

-- least divisor of n, LD(n) 
-- is the same as least divisor starting From a threshold k if k = 2, LDF(k)(n) -- I'm getting a suggestion to change the function to ld = ldf 2, which I think makes sense
ld :: Integral t => t -> t
-- ld n = ldf 2 n
ld = ldf 2

-- "equation guarding", is like a if{}-else-if{}...-else{}
--  the expression condition to the left of the = is the guard
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
        
--1.7
factors :: Integer -> [Integer]
factors n   | n < 1 = error "argument not positive"
            | n == 1 = []
            | otherwise = p : factors (div n p) where p = ld n
            
--1.22
primes0 :: [Integer]
primes0 = filter prime0 [2..]

--1.23 (primes1 is a lazy list)
-- least denominator (only checking primes) starting from some number n
ldpf :: [Integer] -> Integer -> Integer
ldpf [] n               = n          -- BASE CASE: No more primes to check (we shouldn't ever match this, but the linter seems to be happy now)
ldpf (p:ps) n
    | rem n p == 0      = p          -- Match 1: Found the smallest factor
    | p^2 > n           = n          -- Match 2: Current p is too large, n must be prime
    | otherwise         = ldpf ps n  -- Match 3: Continue checking with the rest of the primes

-- least denominator of an int (uses the prime-less-than-the-sqrt-checker helper function above)
ldp :: Integer -> Integer
-- ldp n = ldpf primes1 n --it already returns a function that takes in an integer, so showing the n argument is redundant (1.24) 
ldp = ldpf primes1

-- a new primes definition to avoid a vicious circle in terms of definitions (no ld here)
primes1 :: [Integer]
-- primes1 = 2 : filter prime0 [3..]
primes1 = 2 : filter prime [3..]

-- a more efficient prime checker function
prime :: Integer -> Bool
prime n | n < 1     = error "not a positive integer"
        | n == 1    = False
        | otherwise = ldp n == n



