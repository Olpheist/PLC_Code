module Lecture2 where

import Data.Char

{-------------------------------------------------------------------------------

Today's motivating example is a Caesar, or shift cypher.  But we're going to get
there in several stages.

The first point we need to make is that a Haskell file is a series of
definitions.  A definition contains:

* A type signature
* A series of equations

where each equation has *at least*:

* A left-hand side---the thing we're defining.
* A right-hand side---the body of the definition.

We'll see more of the structure of definitions as we go along...

-------------------------------------------------------------------------------}

message :: String
message = "Attack at dawn"

shiftAmount :: Int
shiftAmount = 3

{-------------------------------------------------------------------------------

The right-hand side need not be a single expression---there could be computation
there.

-------------------------------------------------------------------------------}

biggerShiftAmount :: Int
biggerShiftAmount = shiftAmount + 1

{-------------------------------------------------------------------------------

(+) is actually just a Haskell function like any other... but unlike most
Haskell functions, it's (*by default*) written infix.  Most Haskell functions
are *by default* written prefix:

-------------------------------------------------------------------------------}

shiftedLetter :: Char
shiftedLetter = chr (ord 'a' + biggerShiftAmount)

{-------------------------------------------------------------------------------

We can evaluate Haskell expressions either using the Haskell interpreter `ghci`,
or by using doctest-style comments in our source code.  I'll mostly rely on the
latter for now.

-------------------------------------------------------------------------------}

-- >>> biggerShiftAmount
-- 4

-- >>> shiftedLetter
-- 'e'

{-------------------------------------------------------------------------------

We define our own functions using equations, the same way we define constants.
The left-hand side of such an equation is a *pattern*---you can think of it as
an example of the uses of the thing we're defining:

-------------------------------------------------------------------------------}

shiftConstant :: Char -> Char
shiftConstant c = chr (ord c + shiftAmount)

-- >>> shiftConstant 'a'
-- 'd'

-- >>> shiftConstant 'C'
-- 'F'

{-------------------------------------------------------------------------------

Multiple argument functions are written just the same as single argument
functions---separate arguments with spaces.

(In fact, there's really no such thing as a multiple argument function in
Haskell, but we'll talk about that later...)

-------------------------------------------------------------------------------}

shift :: Int -> Char -> Char
shift n c = chr (ord c + n)

-- >>> shift shiftAmount 'a'

-- >>> shift biggerShiftAmount 'F'

-- Need a `main` function to make Cabal happy
main = return ()
