--Chapter 2
-- Are Haskellers Platonists for accepting the non-constructive proof by contradiction?

-- Connectives (if-then =>, and ^, or V, not -|, if and only if/iff; <=>; (aka 'just in case'?))
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

-- 2.3

