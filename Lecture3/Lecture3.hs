{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Eta reduce" #-}

module Lecture3Full where

import Prelude hiding (pred, reverse)

{- 
-- Our first type is *natural numbers*, or counting numbers.
--
-- The natural numbers are 0, 1, 2, ....  To capture this idea, we rely on the
-- idea of a *successor*: each natural is either 0 or the successor of another
-- natural.
-}

data Nat = Zero | Succ Nat

{-
-- You can think of this as "unary" representation.  Here are some examples:
-}

zero :: Nat
zero = Zero

one :: Nat
one = Succ zero

two :: Nat
two = Succ one

three :: Nat
three = Succ two

{- 
-- Let's define a function 'toInt' that turns a value of type 'Nat'
-- to its counterpart in type 'Int'.
--
-- We can do this in small steps.
-}

-- Start with the type signature and 'undefined'

toIntStep0 :: Nat -> Int
toIntStep0 n = undefined

-- Write a pattern for each constructor

toIntStep1 :: Nat -> Int
toIntStep1 Zero     = undefined
toIntStep1 (Succ n) = undefined

-- Deal with "easy" non-recursive case(s)

toIntStep2 :: Nat -> Int
toIntStep2 Zero     = 0
toIntStep2 (Succ n) = undefined

-- Assume that recursive instances are handled by recursive calls

toIntStep3 :: Nat -> Int
toIntStep3 Zero     = 0
toIntStep3 (Succ n) = let
  _ = n :: Nat
  _ = (toIntStep3 n) :: Int
    in
  undefined -- try to use 'toIntStep3 n', or even 'n' here

-- Handle the recursive case

toIntStep4 :: Nat -> Int
toIntStep4 Zero     = 0
toIntStep4 (Succ n) = let
  _ = n :: Nat
  _ = (toIntStep4 n) :: Int
    in
  1 + toIntStep4 n

-- Let's clean up our scratch work

toInt :: Nat -> Int
toInt Zero     = 0
toInt (Succ n) = 1 + toInt n

{-
-- We can write 'toInt' slightly differently, without surface-level
-- pattern-matching.
-}

isZero :: Nat -> Bool
isZero Zero = True
isZero _    = False

isSucc :: Nat -> Bool
isSucc Zero = False
isSucc _    = True

pred :: Nat -> Nat
pred Zero     = error "Zero does not have a predecessor!"
pred (Succ n) = n

toInt' :: Nat -> Int
toInt' n
  | isZero n = 0
  | isSucc n = 1 + toInt' (pred n)

main :: IO ()
main = return ()
