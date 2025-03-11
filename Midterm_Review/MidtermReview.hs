{-

Exam Format:

pen-and-paper programming questions
knowledge questions
prove properties about programs by using equivalences between Haskell expressions and by using induction

Important Exam Topics:

Evaluation of Haskell expressions, laziness
    Understand well how Haskell expressions are evaluated, and the idea and impact of lazy evaluation.

Guards vs. pattern matching
    Be familiar with the differences between guards and pattern matching. 
    Understand, how guards can be used to deconstruct data and to select a function case.

Recursion to solve problems
    Be able to implement recursive algorithms.  
    Perform recursion over recursive datatypes, such as Lists, or other user-defined datatypes.

Data types and type synonyms
    Be able to define your own datatypes and work with them. 
    Be familiar with the differences between types (data) and type synonyms (type).

Standard functions (map, filter, etc.)
    Be able to use the common standard functions we saw in the lecture.

Typeclasses, instances, and common typeclasses
    Typeclasses are a key concept in Haskell.
    Be familiar with how they function, how they are defined, and how you can implement one for a custom datatype.
    We also saw some common typeclasses. Understanding those are important.

Reasoning about functions
    Haskell differs in a key aspect from other programming languages: functions behave like mathematical functions. 
    They don’t have any side effects. This allows us to reason about Haskell functions, like reasoning about mathematical functions. 
    You should be able to do some simple proofs and use induction.

-}

{-

Regular Expressions

import Prelude hiding (((<>), (<|>), words)

import Data.List ((\\))

-- Regex means regular expression
type Regex = [String]

alphabet :: [Char]
alphabet = ['a'..'z'] ++ ['A'..'Z'] ++ [' ']

char, noChars :: [Char] -> Regex


chars cs = [  [c]  | c <- cs]
notChars cs = [  [c]  | c <- alphabet \\ cs]

anyChars :: Regex
anyChars = chars alphabet

none :: Regex
none = []

empty :: Regex
empty = [""]

(<|>) :: Regex -> Regex -> Regex
r <|> s = r ++ s

cross :: (a -> b -> c) -> [a] -> [b] -> [c]
cross f xs ys = concatMap withY ys where
    withX y x = f x y
    withY y = map (withX y) xs

-- r <|> none = r
plus, question, star :: Regex -> Regex

plus r = r <> (star r)
question r = r <|> empty
star r = question (plus r)

main = return ()

-}

{-

Type Classes

:type (== 5)
:type (== (5 :: Int))

elem :: Eq a => a -> [a] -> Bool
elem x [] = False
elem x (y:ys)
    | x == y    = True
    | otherwise = elem x ys

-- partially applying

:type (elem (5 :: Int))
-- (elem (5 :: Int)) :: [Int] -> Bool

eitherElem :: Eq a => a -> [a] -> [a] -> Bool
eitherElem x xs ys 
    | elem x xs = True
    | elem x ys = True
    | otherwise = False


class Eq a => threeEq a where
    teq :: a -> a -> a -> Bool
    teq x y z
        | x == y && y == z = True
        | otherwise = False
    notteq :: a -> a -> a -> Bool
    notteq x y z = not (teq x y z)

instance threeEq Int where
instance threeEq Char where
instance Eq a => threeEq [a] where
instance threeEq Bool where

    -- teq x y z
    -- below can be a default implementation in the class def
        -- | x == y && y == z = True
        -- | otherwise = False
    --below can be a default implementation in the class def
    -- notteq x y z = not (teq x y z)

teq (5 :: Int) 5 5
--True
teq (5 :: Int) 5 6
--False

--hoogle.haskell.org

map (+ 2) [1, 2, 3, 4]
-- [3, 4, 5, 6]

:intances []

fmap (+ 2) [1, 2, 3, 4]
-- [3, 4, 5, 6]

fmap (+ 2) (Just 3)
-- Just 5

fmap (+ 2) Nothing
-- Nothing

fmap (+ 2) (* 5)
-- well defined, but no instance for 'Show (Integer -> Integer)'

(fmap (+ 2) (* 5)) 3
-- 17

-}