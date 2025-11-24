--Chapter 2
module Math where

-- Are Haskellers Platonists for accepting the non-constructive proof by contradiction?

-- Connectives (if-then =>, and ^, or V, not -|, if and only if/iff; <=>; (aka 'just in case'?))
    -- correct symbols copied in: ¬, ∧, ⇒, ⇔ 

-- predefined
-- data Bool = False | True

-- part of Prelude.hs, the file that contains the predefined Haskell functions.
-- used the book spacing :)
-- not         :: Bool -> Bool
-- not True    = False
-- not False   = True

-- also in Prelude.hs 
-- as a prefix operator definition, (&&) gets the parentheses 
-- but the match definitions use the infix version &&
-- (&&) :: Bool -> Bool -> Bool
-- False   &&  x   = False
-- True    &&  x   = x

-- (||) :: Bool -> Bool -> Bool
-- False || x = x
-- True || x = True

--2.2
-- (P, Q, P XOR Q)
-- (False, False, False)
-- (False, True, True)
-- (True, False, True)
-- (True, True, False)

-- implication
-- antecedent ==> consequent

-- the 1 relates to binding power (0-9) and 'fixity declaration'
infix 1 ==>

(==>) :: Bool -> Bool -> Bool
x ==> y = not x || y

--alternatively
-- (==>) :: Bool -> Bool -> Bool
-- True ==> x = x
-- False ==> _ = True

--because of vacuously true (or 'trivial') statements where antecedent is false, the empty set is a subset of every set

-- Converse: Q ==> P
-- Contrapositive: not Q ==> not P

-- P ==> Q
-- "P is a sufficient condition for Q"
-- "Q is a necessary condition for P"
-- if P, then Q
-- Q if P
-- P only if Q
-- Q whenever P

-- P <==> Q
-- Equivalence
-- iff
-- P ==> Q && Q ==> P
-- P ==> Q && P <== Q
-- "P is necessary and sufficient for Q"

-- Equivalence
-- There is no need to add a definition of a function for equivalence to Haskell.
-- type Bool is in class Eq, which means that an equality relation is predefined on it.
-- equivalence of propositions is nothing other than equality of their truth values
-- so this is a synonym that should act the same way
infix 1 <=>

(<=>) :: Bool -> Bool -> Bool
x <=> y = x == y

-- 2.4
-- exclusive or 
-- I guess /= is not equals 
-- Q: what does infixr mean compared to infix?
-- A: right associative (i.e. a <+> b <+> c is parsed as a <+> (b <+> c))
-- similarly, for infixl
-- infix is non-associative; a == b == c is a syntax error; you must write (a == b) == c or a == (b == c)
-- Q: how does a 2 compar to a 1?
-- A: Higher Number = Tighter Binding (Higher Precedence):
-- An operator with a higher number will be evaluated before an operator with a lower number, when the two operators appear side-by-side in an expression without parentheses.
infixr 2 <+>

(<+>) :: Bool -> Bool -> Bool
x <+> y = x /= y

-- ¬ P ∧ ((P ⇒ Q) ⇔ ¬ (Q ∧ ¬ P))
--   t     t   f       f     t
-- f         f             f
--                       f
--                  t
--               f
--     f
-- 
-- f t f   t f f f  t  f f f t

p :: Bool
p = True

q :: Bool
q = False

-- p and q are constants
formula1 :: Bool
formula1 = (not p) && (p ==> q) <=> not (q && (not p))

--logically valid propositional formulas evaluate to t no matter the values

-- p and q are variables
formula2 :: Bool -> Bool -> Bool
formula2 p q = (not p) && (p ==> q) <=> not (q && (not p))


-- validity check for just one variable (bf is Boolaean Function)
valid1 :: (Bool -> Bool) -> Bool
valid1 bf = bf True && bf False

-- excluded middle; no third possibility
-- P ∨ ¬P
excludedMiddle :: Bool -> Bool
excludedMiddle p = p || not p

-- *Main> valid1 excludedMiddle
-- True


-- validity check for two variable (bf is Boolaean Function)
valid2 :: (Bool -> Bool -> Bool) -> Bool
valid2 bf = (bf True True)
            && (bf True False)
            && (bf False True)
            && (bf False False)

form1 :: Bool -> Bool -> Bool
form1 p q = p ==> (q ==> p)

form2 :: Bool -> Bool -> Bool
form2 p q = (p ==> q) ==> p

-- *Main> valid2 form1
-- True
-- *Main> valid2 form2
-- False
-- *Main> valid2 formula2
-- False

-- bf is a Boolean Function 3 and 4?
--  p <- [True,False]   
-- “p is an element of the list consisting of the two truth values”, is an example of list comprehension
-- and is predefined conjunction 
valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 bf = and [ bf p q r | p <- [True,False],
                             q <- [True,False],
                             r <- [True,False]]

valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 bf = and [ bf p q r s | p <- [True,False],
                               q <- [True,False],
                               r <- [True,False],
                               s <- [True,False]]

-- Operator Precedence; binding order (precedence)
-- == binds more strongly
-- == (4), && (3), || (2), ==> <=> (1)

-- 'logically equivalent' = same truth table
-- you can check De Morgan's Law ¬ (P ∧ Q) ⇔ (¬ P ∨ ¬ Q)

-- ⊕ = xor

-- 2.9
-- (P ⊕ Q) ⊕ Q is equivalent to P  (imagine Screen and Cursor as bits)

-- propositional function of one parameter
-- Φ, Ψ with a single propositional variable, is testing the formula Φ ⇔ Ψ by the truth table method.
-- e.g.
-- Φ, bf1: True -> False, False -> True
-- Ψ, bf2: T -> F, F -> F
-- function checks if they both act the same under True and then False inputs 
-- the example above would be False because Φ, Ψ return different values on False inputs
logEquiv1 :: (Bool -> Bool) -> (Bool -> Bool) -> Bool
logEquiv1 bf1 bf2 =
    (bf1 True <=> bf2 True) && (bf1 False <=> bf2 False)

-- we can extend it to Boolean Functions with more arguments using a list comprehension
-- Boolean Function of two input arguments
logEquiv2 :: (Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool) -> Bool
logEquiv2 bf1 bf2 = and [bf1 p q <=> bf2 p q | p <- [True,False], q <- [True,False]]

-- the | seems to act as a "for" in a list comprehension instead of a "guard" in a function
-- this function iterates over all 2^3 = 8 possible True/False input possibilities F F F, F F T, etc.
logEquiv3 :: (Bool -> Bool -> Bool -> Bool) -> (Bool -> Bool -> Bool -> Bool) -> Bool
logEquiv3 bf1 bf2 = and [ bf1 p q r <=> bf2 p q r | p <- [True,False],
                                                    q <- [True,False],
                                                    r <- [True,False]]

-- "by computer"
--  show (P ⊕ Q) ⊕ Q is equivalent to P
-- P=F Q=F -> F 
-- P=F Q=T -> F 
-- P=T Q=F -> T 
-- P=T Q=T -> T 

-- P
-- needs two arguments even though we just take P, like above
formula3 :: Bool -> Bool -> Bool
formula3 p q = p

-- (P ⊕ Q) ⊕ Q
formula4 :: Bool -> Bool -> Bool 
formula4 p q = (p <+> q) <+> q


-- *Main> logEquiv2 formula3 formula4
-- True



formula5 :: Bool -> Bool -> Bool
formula5 p q = p <=> ((p <+> q) <+> q)

-- truth table test:
-- remember, logically valid propositional formulas evaluate to True no matter the Boolean input values
-- validity check that a Boolean Function of Two variables checks that all 4 possible inputs evaluate to True
-- valid2 :: (Bool -> Bool -> Bool) -> Bool
-- valid2 bf = (bf True True)
--             && (bf True False)
--             && (bf False True)
--             && (bf False False)

-- *Main> valid2 formula5
-- True

-- 2.10
-- A number of useful equivalences
-- And P, Q, and R can be arbitrary formulas themselves

-- 1. P ≡ ¬¬P (law of double negation),
-- 2. P ∧ P ≡ P; P ∨ P ≡ P (laws of idempotence),
-- 3. (P ⇒ Q) ≡ ¬P ∨ Q;
-- ¬(P ⇒ Q) ≡ P ∧ ¬Q,
-- 4. (¬P ⇒ ¬Q) ≡ (Q ⇒ P);
-- (P ⇒ ¬Q) ≡ (Q ⇒ ¬P);
-- (¬P ⇒ Q) ≡ (¬Q ⇒ P) (laws of contraposition),
-- 5. (P ⇔ Q) ≡ ((P ⇒ Q) ∧ (Q ⇒ P))
-- ≡ ((P ∧ Q) ∨ (¬P ∧ ¬Q)),
-- 6. P ∧ Q ≡ Q ∧ P; P ∨ Q ≡ Q ∨ P (laws of commutativity),
-- 7. ¬(P ∧ Q) ≡ ¬P ∨ ¬Q;
-- ¬(P ∨ Q) ≡ ¬P ∧ ¬Q (DeMorgan laws).
-- 8. P ∧ (Q ∧ R) ≡ (P ∧ Q) ∧ R;
-- P ∨ (Q ∨ R) ≡ (P ∨ Q) ∨ R (laws of associativity),
-- 9. P ∧ (Q ∨ R) ≡ (P ∧ Q) ∨ (P ∧ R);
-- P ∨ (Q ∧ R) ≡ (P ∨ Q) ∧ (P ∨ R) (distribution laws),


-- 2.11
-- (Truth Table created in 2.6 ✅)
-- ¬(P ∧ Q) ≡ ¬P ∨ ¬Q 

-- the other part of DeMorgan's law:
-- ¬(P ∨ Q) ≡ ¬P ∧ ¬Q 

-- P | Q | not (P || Q) | not P && not Q
-- F   F        T               T
-- F   T        F               F
-- T   F        F               F
-- T   T        F               F

-- λp.¬¬p
-- \ p -> not (not p)

test1 :: Bool
test1 = logEquiv1 id (\ p -> not (not p))

test2a :: Bool
test2a = logEquiv1 id (\ p -> p && p)

test2b :: Bool
test2b = logEquiv1 id (\ p -> p || p)

test3a :: Bool
test3a = logEquiv2 (\ p q -> p ==> q) (\ p q -> not p || q)

test3b :: Bool
test3b = logEquiv2 (\ p q -> not (p ==> q)) (\ p q -> p && not q)

test4a :: Bool
test4a = logEquiv2 (\ p q -> not p ==> not q) (\ p q -> q ==> p)

test4b :: Bool
test4b = logEquiv2 (\ p q -> p ==> not q) (\ p q -> q ==> not p)

test4c :: Bool
test4c = logEquiv2 (\ p q -> not p ==> q) (\ p q -> not q ==> p)

test5a :: Bool
test5a = logEquiv2 (\ p q -> p <=> q) (\ p q -> (p ==> q) && (q ==> p))

test5b :: Bool
test5b = logEquiv2 (\ p q -> p <=> q) (\ p q -> (p && q) || (not p && not q))

test6a :: Bool
test6a = logEquiv2 (\ p q -> p && q) (\ p q -> q && p)

test6b :: Bool
test6b = logEquiv2 (\ p q -> p || q) (\ p q -> q || p)

test7a :: Bool
test7a = logEquiv2 (\ p q -> not (p && q)) (\ p q -> not p || not q)

test7b :: Bool
test7b = logEquiv2 (\ p q -> not (p || q)) (\ p q -> not p && not q)

test8a :: Bool
test8a = logEquiv3 (\ p q r -> p && (q && r)) (\ p q r -> (p && q) && r)

test8b :: Bool
test8b = logEquiv3 (\ p q r -> p || (q || r)) (\ p q r -> (p || q) || r)

test9a :: Bool
test9a = logEquiv3 (\ p q r -> p && (q || r)) (\ p q r -> (p && q) || (p && r))

test9b :: Bool
test9b = logEquiv3 (\ p q r -> p || (q && r)) (\ p q r -> (p || q) && (p || r))

-- *Main> test5a
-- True
-- *Main> test1
-- True

-- T (the proposition that is always true; the Haskell counterpart is True) and 
-- ⊥ (the proposition that is always false; the Haskell counterpart of this is False).

-- 1. ¬T ≡ ⊥; ¬⊥ ≡ >,
-- 2. P ⇒ ⊥ ≡ ¬P,
-- 3. P ∨ T ≡ T; P ∧ ⊥ ≡ ⊥ (dominance laws),
-- 4. P ∨ ⊥ ≡ P; P ∧ T ≡ P (identity laws),
-- 5. P ∨ ¬P ≡ T (law of excluded middle),
-- 6. P ∧ ¬P ≡ ⊥ (contradiction).


-- substitution principle example:
-- ¬(P ⇒ Q) ≡ P ∧ ¬Q 
-- substituting ¬P for P
--  substituting "a = 2^b − 1" for P and "a is prime" for Q
-- ¬(a = 2^b − 1 ⇒ a is prime) ≡ a = 2^b − 1 ∧ a is not prime

-- 2.15
-- Testing for contradictions for proposition variable (Bool -> Bool) (these take one input)
-- We'll test all possible inputs (True, False) to make sure they all evaluate to False
-- e.g. propTest q = q <=> not q
contradictionTest1 :: (Bool -> Bool) -> Bool
contradictionTest1 bf = not (bf True) && not (bf False)

propTest :: Bool -> Bool
propTest p = p <=> not p

-- *Main> contradictionTest1 propTest
-- True

contradictionTest2 :: (Bool -> Bool -> Bool) -> Bool
contradictionTest2 bf = not (bf False False) && 
                        not (bf False True) &&
                        not (bf True False) &&
                        not (bf True True)

propTest2 :: Bool -> Bool -> Bool
propTest2 p q = p && q <=> not (p && q)

-- *Main> contradictionTest2 propTest2
-- True


-- 2.17
denialXYZ :: Double -> Double -> Double -> Bool
denialXYZ x y z = not (x < y && y < z)

-- *Main> denialXYZ 5 6 7
-- False
-- *Main> denialXYZ 5 5 5
-- True
-- *Main> denialXYZ 5.5 6.65 7.767
-- False
-- *Main> denialXYZ 5.55 5.233 5.55
-- True

-- 2.18
-- Φ,Ψ,¬Φ,¬Ψ,Φ⇔Ψ,¬Φ⇔¬Ψ
-- T,T,F,F,T,T
-- T,F,F,T,F,F
-- F,T,T,F,F,F
-- F,F,T,T,T,T

-- Φ,Ψ,¬Φ,¬Ψ,¬Φ⇔Ψ,Φ⇔¬Ψ
-- T,T,F,F,F,F
-- T,F,F,T,T,T
-- F,T,T,F,T,T
-- F,F,T,T,F,F

-- 2.19

-- We won't show Φ ≡ Ψ is true iff Φ ⇔ Ψ is logically valid, 
-- but if we were to, we would show that assuming one implies the other in both directions

-- Logical Equivalence,
-- Φ≡Ψ,
-- Φ and Ψ have the same truth value in every possible row of the truth table.
-- So the propositions should same number of arguments and the same overall True False value in each input possibility.

-- Logical Validity
-- Φ⇔Ψ
-- The formula Φ⇔Ψ is a tautology (always True in every possible row of the truth table).

-- 2.20
-- which are equivalent?

-- 1. ¬P ⇒ Q and P ⇒ ¬Q,
test2_20_1 :: Bool
test2_20_1 = logEquiv2 (\ p q -> not p ==> q) (\ p q -> p ==> not q)

-- *Main> test2_20_1
-- False


-- 2. ¬P ⇒ Q and Q ⇒ ¬P,

test2_20_2 :: Bool
test2_20_2 = logEquiv2 (\ p q -> not p ==> q) (\ p q -> q ==> not p)

-- *Main> test2_20_2
-- False

-- 3. ¬P ⇒ Q and ¬Q ⇒ P,

test2_20_3 :: Bool
test2_20_3 = logEquiv2 (\ p q -> not p ==> q) (\ p q -> not q ==> p)

-- *Main> test2_20_3
-- True

-- 4. P ⇒ (Q ⇒ R) and Q ⇒ (P ⇒ R),

test2_20_4 :: Bool
test2_20_4 = logEquiv3 (\ p q r -> not p ==> (q ==> r)) (\ p q r -> q ==> (p ==> r))

-- 5. P ⇒ (Q ⇒ R) and (P ⇒ Q) ⇒ R,

test2_20_5 :: Bool
test2_20_5 = logEquiv3 (\ p q r -> not p ==> (q ==> r)) (\ p q r -> (p ==> q) ==> r)

-- 6. (P ⇒ Q) ⇒ P and P,

test2_20_6 :: Bool
test2_20_6 = logEquiv2 (\ p q -> (p ==> q) ==> p) (\ p q -> p)

-- 7. P ∨ Q ⇒ R and (P ⇒ R) ∧ (Q ⇒ R).

test2_20_7 :: Bool
test2_20_7 = logEquiv3 (\ p q r -> (p || q) ==> r) (\ p q r -> (p ==> r) && (q ==> r))

-- *Main> test2_20_4
-- False
-- *Main> test2_20_5
-- False
-- *Main> test2_20_6
-- True
-- *Main> test2_20_7
-- True

exercise2_21_1 :: Bool -> Bool -> Bool
exercise2_21_1 x y  | not x && y = False
                    | otherwise = True

-- *Main> exercise2_21_1 False False
-- True
-- *Main> exercise2_21_1 False True
-- False
-- *Main> exercise2_21_1 True False
-- True
-- *Main> exercise2_21_1 True True
-- True

-- 2.21.2, 2.21.3
-- There are 4 rows, each of them could be true or false overall, 
-- so there are 2^4 = 16 truth tables for formulas with two variables
-- here's the first:
-- PQΦ
-- FFF
-- FTF
-- TFF
-- TTF

-- 2.21.4 Formulas of two variables could be any length, so enumerate symbols and start with the shorter ones
-- 2.21.5 Formulas of three variables could be any length, so enumerate symbols and start with the shorter ones

-- ∀x,z∈Q ( x < z ⇒ ∃y∈Q ( x < y ∧ y < z ) ).
-- 2.22 Proof: Let y = (x + z) / 2

-- ∀, universal quantifier; for all
-- ∃, existential quantifier; there exists

-- N for the natural numbers, Z for the integer numbers, Q for the rational numbers, and R for reals

-- 2.26
-- 1. ∃x,y ∈ Q(x < y) ...

-- 2.27
-- 1. ∀ x ∈ Q ∃ m,n ∈ Z(x = m/n).

--Bound Variable
-- ∃y ∈ Q(x < y) has the same meaning as ∃z ∈ Q(x < z)
-- ∃y ∈ Q(x < y) has a different meaning ∃y ∈ Q(z < y)

-- Prelude> sum [ i | i <- [1..5] ]
-- 15
-- Prelude> sum [ n | n <- [1..5] ]
-- 15

-- 2.31.1
-- ∃x ∈ C (x^2 + 1 = 0)

-- 2.32.1
-- ∀x ∈ People (L(x,d))

-- 2.36.1
-- There exist x in the Reals such that x squared equals 5.

square1 :: Integer -> Integer
square1 x = x^2

square2 :: Integer -> Integer
square2 = \ x -> x^2

-- Either works as a lambda function for passing arguments; second is better
m1 :: Integer -> Integer -> Integer
m1 = \ x -> \ y -> x*y

m2 :: Integer -> Integer -> Integer
m2 = \ x y -> x*y

solveQdr :: (Float,Float,Float) -> (Float,Float)
solveQdr = \ (a,b,c) -> if a == 0 then error "not quadratic"
                        else let d = b^2 - 4*a*c in
                        if d < 0 then error "no real solutions"
                        else ((- b + sqrt d) / 2*a, (- b - sqrt d) / 2*a)

-- *Main> solveQdr (1, -1, -1)
-- (1.618034,-0.618034)

-- predicate name P for the primes
-- P(n) :≡ n > 1 ∧ ∀m((1 < m < n) ⇒ ¬m|n). 
-- P(n) :≡ n > 1 ∧ ∀m((1 < m ∧ m^2 <= n) ⇒ ¬m|n).
-- P(n) :≡ n > 1 ∧ LD(n) = n

-- 2.6 Abstract Formulas and Concrete Structures

-- what is R operator, or context or structure? what is the domain of quantification? The truth value depends on that.
-- ∀x ∀y ( xRy =⇒ ∃z ( xRz ∧ zRy ) ).

-- Free variables
-- An open formula can be turned into a statement in two ways: 
-- (i) adding quantifiers that bind the free variables; 
-- (ii) replacing the free variables by (names of) objects in the domain (or stipulating that they have such objects as values).

-- 2.37.a 
-- 1) False
-- 2) True 
-- 3) True (0)

-- 2.43 continuity
-- ∀x ∀ε > 0 ∃δ > 0 ∀y ( |x − y| < δ ==> |f(x) − f(y)| < ε ).

--2.46 Not the same; Let A be the subset of numbers less than 2 and Φ(x) be isPrime
-- ¬∃x ∈ A Φ(x) is not equivalent to ∃x not-∈ A Φ(x)
-- ¬∃x ∈ A Φ(x) says there isn't a number less than 2 that is prime, which is true
-- ∃x not-∈ A Φ(x) says there exists a number greater than or equal to 2 that is prime, which is also true but a different statement

-- 2.49 discontinuous 
-- ¬ ∀ε > 0 ∃δ > 0 ∀y ( |x − y| < δ =⇒ |f(x) − f(y)| < ε ).
-- ∃ε > 0 ∀δ > 0 ∃y ¬ ( |x − y| < δ =⇒ |f(x) − f(y)| < ε ).
-- ∃ε > 0 ∀δ > 0 ∃y ( |x − y| < δ ∧ |f(x) − f(y)| > ε ).

-- 2.50
-- a0, a1, a2, . . . ∈ R converges to a:lim n→∞ a_n = a, means that 
-- ∀δ > 0,∃n,∀m > n, (|a − a_m| < δ)
-- Doesn't converge:
-- ¬ (∀δ > 0, ∃n, ∀m > n, (|a − a_m| < δ))
-- use the following equivalences to make a positive equivalent of the statement without the not:
-- ¬ (∀x P(x)) <=> ∃x ¬P(x)
-- ¬ (∃x, P(x)) <=> ∀x ¬P(x)
-- ¬ (a < b) <=> (a >= b)
--
-- ∃δ > 0, ∀n, ∃m > n, (|a − a_m| < δ))
--
-- Translation:
-- There exists a specific distance delta such that, no matter how far down the sequence you go (for all n), 
-- you can always find a later term (a_m) that is at least delta distance away from a.

-- 2.8 Quantifiers as procedures
-- If we implement sets as lists, it is straightforward to implement ∀ (all) and ∃ (any)

-- pass the function that takes type a and checks for property, pass the list of a, and get back a True/False of whether it holds for any or all
-- any, all :: (a -> Bool) -> [a] -> Bool
-- any p = or . map p
-- all p = and . map p

-- why not "all p"? (it has "all" built in already)
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

-- and :: [Bool] -> Bool
-- all elements of a list xs satisfy a property p = map p xs contains only True 
-- above, it is first apply map p, then apply (with composition "and after map" .) and

-- why not "any p" suggestion pops up; it has "any" built in already
any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

-- or :: [Bool] -> Bool
-- some element of a list xs satisfy a property p = map p xs contains at least one True 
-- above, it is first apply map p, then apply or

-- Prelude> any (<3) [0..]
-- True
-- Prelude>  all (<3) [0..]
-- False

-- similar, but the argument input is reversed
every, some :: [a] -> (a -> Bool) -> Bool
every xs p = all p xs
some xs p = any p xs


-- ∀x ∈ {1, 4, 9}, ∃y ∈ {1, 2, 3} (x = y^2)
-- *Main> every [1,4,9] (\ x -> some [1,2,3] (\ y -> x == y^2))
-- True

-- runs forever. the quantifiers are procedures, not "algorithms" (that terminate?)
-- every [0..] (>=0)

-- 2.51
unique :: (a -> Bool) -> [a] -> Bool
unique p xs = length (filter p xs) == 1

--2.52
parity :: [Bool] -> Bool
parity xs = even (length (filter (== True) xs))

-- 2.53
evenNR :: (a -> Bool) -> [a] -> Bool
evenNR p xs = parity (map p xs)

