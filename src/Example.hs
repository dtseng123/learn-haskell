-- Example module
-- Gabriele Keller, August 2015
--
-- This is a simple example of a module definition

module Simple
where

-- calculates the arithmetic mean of two numbers
--
arithmetic_mean :: Fractional a => a -> a -> a
arithmetic_mean x y  = (x + y)  / 2

-- calculates the harmonic mean of two numbers
--
harmonic_mean :: Fractional a => a -> a -> a
harmonic_mean x y  = 2 * x * y / (x + y)