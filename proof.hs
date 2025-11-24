-- Chapter 3
module Proof where
import Math

-- *Main> :r
-- [1 of 2] Compiling Math             ( Math.hs, interpreted )
-- [2 of 2] Compiling Proof            ( proof.hs, interpreted )
-- Ok, two modules loaded.

-- proof best practices
-- sub-proofs

-- Introduction Rule (for implications is the Deduction Rule)

-- Given: . . .
-- To be proved: Φ ⇒ Ψ
-- Proof:
    -- Suppose Φ
    -- To be proved: Ψ
    -- Proof: . . .
-- Thus Φ ⇒ Ψ.

-- Elimination Rule (Modus Ponens) 
    -- Given: Φ ⇒ Ψ, Φ
    -- Thus Ψ.

-- Safety. Immediate from truth tables: if Φ ⇒ Ψ and Φ are both true, then so is Ψ.

-- A given structure of proof (we show if P is assumed (along with the givens) R follows):
--
-- Given: P ⇒ Q, Q ⇒ R
-- To be proved: P ⇒ R
-- Proof:
    -- Suppose P
    -- To be proved: R
    -- Proof: From P ⇒ Q and P, conclude Q.
    -- Next, from Q ⇒ R and Q, conclude R.
-- Thus P ⇒ R

-- 3.2
-- Given: P ⇒ Q, P ⇒ (Q ⇒ R)
-- To be proved: P ⇒ R
-- Proof:
    -- Suppose P
    -- To be proved: R
    -- Proof: 
    -- From P ⇒ Q and P, conclude Q.
    -- From P ⇒ (Q ⇒ R) and P, conclude (Q ⇒ R)
    -- From Q and (Q ⇒ R), conclude R
-- Thus P ⇒ R


-- Conjunction
-- Introduction:
    -- Given: Φ, Ψ
    -- Thus Φ ∧ Ψ.
-- Elimination:
    -- Given: Φ ∧ Ψ
    -- Thus Φ.

    -- Given: Φ ∧ Ψ
    -- Thus Ψ.

-- never stops generating evens
evens :: [Integer]
-- evens = [ x | x <- [0..10], even x ] --This will terminate and return True
evens = [ x | x <- [0..], even x ]

-- forall [ m + n | m <- evens, n <- evens ] even
checkAllEven :: Bool
checkAllEven = every [ m + n | m <- evens, n <- evens ] even --This will not terminate

-- but a finite proof exists
-- Concise proof:
--     Assume that m and n are even.
--     For instance, m = 2p, n = 2q, p, q ∈ N.
--     Then m + n = 2p + 2q = 2(p + q) is even.

-- 3.4
--  Assume that n, m ∈ N.
-- Show: (m is odd ∧ n is odd) ⇒ m + n is even

-- Concise proof:
--     Assume that m and n are odd.
--     For instance, m = 2p + 1, n = 2q + 1, p, q ∈ N.
--     Then m + n = 2p + 1 + 2q + 1 = 2(p + q + 1) is even.

-- 3.5 iff is a two part proof
-- 1. From P ⇔ Q it follows that (P ⇒ R) ⇔ (Q ⇒ R),

-- Given P ⇔ Q 
-- To be proved: (P ⇒ R) ⇔ (Q ⇒ R)

-- Proof:

-- Part 1: First Direction
-- Assume: P ⇒ R
-- Show:   Q ⇒ R
-- From P ⇔ Q we have both P ⇒ Q and Q ⇒ P.
--     Assume Q.
--     From Q ⇒ P and Q, conclude P.
--     From P ⇒ R and P, conclude R.
-- Thus Q implies R.
-- Therefore (P ⇒ R) ⇒ (Q ⇒ R).

-- Part 2: Other Direction
-- Assume: Q ⇒ R
-- Show:   P ⇒ R
-- From P ⇔ Q we have both P ⇒ Q and Q ⇒ P.
--     Assume P.
--     From P ⇒ Q and P, conclude Q.
--     From Q ⇒ R and Q, conclude R.
-- Thus P implies R.
-- Therefore (Q ⇒ R) ⇒ (P ⇒ R).

-- Hence (P ⇒ R) ⇔ (Q ⇒ R).
