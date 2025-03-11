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

Hutton Exercises

Chapter 1
----------------------------------------
double x = x + x

double (double 2)
=       {applying the inner double}
    double (2 + 2)
=       {applying +}
    double 4
=       {applying double}
    4 + 4
=       {applying +}
    8

double (double 2)
=       {applying the outer double}
    double 2 + double 2
=       {applying the first double}
    (2 + 2) + double 2
=       {applying the first +}
    4 + double 2
=       {applying double}
    4 + (2 + 2)
=       {applying the second +}
    4 + 4
=       {applying +}
    8

double (double 2)
=       {applying the inner double}
    double (2 + 2)
=       {applying double}
    (2 + 2) + (2 + 2)
=       {applying first +}
    4 + (2 + 2)
=       {applying second +}
    4 + 4
=       {applying +}
    8

double (double 2)
=       {applying the outer double}
    double 2 + double 2
=       {applying the first double}
    (2 + 2) + double 2
=       {applying first +}
    4 + double 2
=       {applying double}
    4 + 2 + 2
=       {applying first +}
    6 + 2
=       {applying +}
    8

----------------------------------------
sum []     = 0
sum (n:ns) = n + sum ns

    sum [x]
=       {applying sum}
    x + sum []
=       {applying sum}
    x + 0
=       {applying +}
    x
----------------------------------------
product :: Num a => [a] -> a
product [] = 1
product (n:ns) = n * product ns

product [2,3,4]
=   2 * product [3,4]
=   2 * 3 * product [4]
=   2 * 3 * 4 * product []
=   2 * 3 * 4 * 1
=   24
----------------------------------------
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
    where
        smaller = [a | a <- xs, a <=x]
        larger = [b | b <- xs, b > x]

qsort [3, 5, 4, 1, 2]
qsort [5, 4] ++ [3] ++ qsort [1,2]
(qsort [] ++ [5] ++ qsort [4]) ++ [3] ++ qsort [1,2]
([] ++ [5] ++ (qsort [] ++ [4] ++ qsort [])) ++ [3] ++ qsort [1,2]
[5,4,3] ++ (qsort [2] ++ [1] ++ qsort [])
[5,4,3] ++ (qsort [] ++ [2] ++ qsort []) ++ [1]
[5,4,3,2,1]
----------------------------------------
qsort [2,2,3,1,1]
qsort [1,1] ++ [2] ++ qsort [3]
(qsort [] ++ [1] ++ qsort []) += [2] + qsort [3]
You would lose the replicate elements

-}