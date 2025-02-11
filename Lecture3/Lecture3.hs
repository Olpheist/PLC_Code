{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Eta reduce" #-}

module Lecture3 where

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

{-
-- Now we want GHCi to print each 'Nat' value as an 'Int'
-- automatically. We're going to define a 'Show' instance
-- for 'Nat'. This is a lot like Python's '__repr__()'.
-}

instance Show Nat where
   -- In general, 'show' has type 'a -> String'
   -- The instance of 'show' we're writing has type 'Nat -> String'
   -- The instance of 'show' we're calling has type 'Int -> String'
   show n = let
    showInt = show :: Int -> String
      in
    showInt (toInt n) ++ "N"

{-
-- We can also go from non-negative 'Int' values to 'Nat' values.
-}

toNat :: Int -> Nat
toNat i
  | i <  0 = error "negative integers are not natural numbers"
  | i == 0 = Zero
  | i >  0 = Succ (toNat (i - 1))

{-
-- Let's acquire an intuition for adding two values of type 'Nat'
-- before we write a function to implement addition.
--
-- We can think of each natural number has a stack of pancakes.
-- 
-- 'Zero' is an empty bowl. Each 'Succ' is a pancake.
--
--                               **** Succ
--                  **** Succ    **** Succ
--                  **** Succ    **** Succ
--                  **** Succ    **** Succ
--     \__/ Zero    \__/ Zero    \__/ Zero
--      0            3            4
--
-- The 'plus n m' operation represents transferring all the pancakes
-- one by one from a bowl with 'n' pancakes to a bowl with 'm' pancakes,
-- or the other way around.
--
-- A reasonable algorithm is 
--
-- 1. check if the bowl we're moving pancakes FROM is empty already,
--    if it is then we must be finished!
-- 2. if the bowl we're moving pancakes FROM still has at least one
--    pancake, we can take that pancake off the top and move it to the
--    other bowl
-}

plus_m_to_n :: Nat -> Nat -> Nat
plus_m_to_n m n
  | (isSucc m) = let
    m' = pred m
    n' = Succ n
      in
    plus_m_to_n m' n'
  | (isZero m) = n    

plus_n_to_m :: Nat -> Nat -> Nat
plus_n_to_m m Zero     = m
plus_n_to_m m (Succ k) = let
  _ = m :: Nat
  _ = k :: Nat
    in
  plus_n_to_m (Succ m) k

{- 
-- Another algorithm encourages us to imagine *three* bowls of pancakes.
-- Two bowls are "input" bowls and a third bowl is the "output" bowl.
-- 
-- 1. if both input bowls are empty, we must be finished!
-- 2. if the first input bowl is empty and the second has a single pancake,
--    transfer the single pancake to the output bowl
-- 3. if the first input bowl has a single pancake and the second bowl is
--    empty, transfer the single pancake to the output bowl
-- 4. if both input bowls have at least one pancake each, take one pancake
--    from each input bowl and move it to the output bowl
-}

plus_both :: Nat -> Nat -> Nat
plus_both (Succ m) (Succ n) = Succ (Succ (plus_both m n))
plus_both (Succ m) Zero     = Succ (plus_both m Zero)
plus_both Zero     (Succ n) = Succ (plus_both Zero n)
plus_both Zero     Zero     = Zero

{- 
-- Let's now define a 'monus' function. It's best described by eating
-- pancakes. Suppose I have a stack of 3 pancakes, and I want to eat
-- 4 pancakes. I can't be left with -1 pancakes. The least I can be
-- left with is 0. 'monus 3 4' equals 0.
--
-- An algorithm for monus:
--
-- 1. if I have no pancakes in my bowl even if I'm still hungry, I'm
--    done! 
-- 2. if I have 'n' pancakes in my bowl and I'm no longer hungry, I'm
--    done!
-- 3. if I have at least one pancake in my bowl and I want to eat at
--    least one more pancake, I eat it, removing one pancake from my
--    bowl and reducing my hunger by one pancake
-}

monus :: Nat -> Nat -> Nat
monus m n
  | (isSucc m) && (isZero n) = m
  | (isZero m) && (isSucc n) = m -- Zero
  | (isZero m) && (isZero n) = m -- also Zero
  | (isSucc m) && (isSucc n) = monus (pred m) (pred n)

{-
-- Here is a very similar definition of 'monus' that uses explicit
-- pattern-matching.
-}

monus' :: Nat -> Nat -> Nat
monus' m        Zero     = m
monus' Zero     n        = Zero
monus' (Succ m) (Succ n) = monus' m n 

{-
-- A general pattern for recursive functions over 'Nat'.
-}

recNat :: (b -> b) -> b -> Nat -> b
recNat whatIfSucc whatIfZero Zero     = whatIfZero
recNat whatIfSucc whatIfZero (Succ n) = whatIfSucc (recNat whatIfSucc whatIfZero n)

{-
-- Yet another implementation of 'toInt', this time using 'recNat'.
-}

toInt'' :: Nat -> Int
toInt'' n = recNat (+ 1) 0 n

main :: IO ()
main = return ()
