module Lecture2 where

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