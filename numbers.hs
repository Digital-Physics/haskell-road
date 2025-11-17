-- |===================================================================|
-- |                      HASKELL NUMERIC TYPE CLASSES                 |
-- |===================================================================|

-- Type Classes define interfaces or contracts that specific types must
-- satisfy. They group types based on the operations they support.
-- Note: A Type Class (e.g., Integral) is NOT a concrete type.

-- Class: Num
-- Description: The most general numeric class. Members support basic arithmetic (+, -, *) and absolute value (abs).
-- Basic Types: Int, Integer, Float, Double
--
--    -- Example function constrained by Num:
--    addThree :: Num a => a -> a -> a -> a
--    addThree x y z = x + y + z

-- Class: Real
-- Description: Members that are ordered real numbers (support comparison).
-- Basic Types: Int, Integer, Float, Double

-- Class: Integral
-- Description: The class for whole-number types. Members support integer-specific operations like division that returns both quotient and remainder (divMod).
-- Basic Types: Int, Integer
--
--    -- Example function constrained by Integral:
--    myDivMod :: Integral a => a -> a -> (a, a)
--    myDivMod = divMod

-- Class: Fractional
-- Description: The class for fractional (floating-point) types. Members support floating-point division (/).
-- Basic Types: Float, Double
--
--    -- Example function constrained by Fractional:
--    safeDiv :: Fractional a => a -> a -> a
--    safeDiv = (/)

-- Class: Floating
-- Description: Members support transcendental functions like sin, cos, and exp.
-- Basic Types: Float, Double
--
--    -- Example function constrained by Floating:
--    calcSin :: Floating a => a -> a
--    calcSin = sin