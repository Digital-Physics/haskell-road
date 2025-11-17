--Chapter 2
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
valid1 bf = (bf True) && (bf False)

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


valid3 :: (Bool -> Bool -> Bool -> Bool) -> Bool
valid3 bf = and [ bf p q r | p <- [True,False],
                             q <- [True,False],
                             r <- [True,False]]

valid4 :: (Bool -> Bool -> Bool -> Bool -> Bool) -> Bool
valid4 bf = and [ bf p q r s | p <- [True,False],
                               q <- [True,False],
                               r <- [True,False],
                               s <- [True,False]]