import Prelude hiding ((<>), (<|>), words)

import Data.List ((\\))

-- Regex means regular expression
type Regex = [String]

alphabet :: [Char]
alphabet = ['a'..'z'] ++ ['A'..'Z'] ++ [' ']

chars, notChars :: [Char] -> Regex
chars cs = [  [c]  | c <- cs]
notChars cs = chars (alphabet \\ cs)

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

r <> s = cross (++) r s 

-- r <|> none = r
plus, question, star :: Regex -> Regex

plus r = r <> (star r)
question r = empty <|> r
star r = question (plus r)

--------------------------------
upper, lower, space :: Regex
upper = chars ['A'..'Z']
lower = chars ['a'..'z']
space = chars [' ']

word = upper <> star lower
words = word <> star (space <> word)
--------------------------------

data DRegex = AnyChars | Chars [Char] | NotChars [Char] |
    DRegex:|: DRegex | None | DRegex :<>: DRegex | Empty | Question DRegex |
    Plus DRegex | Star DRegex
        deriving (Show)

dupper :: Regex
dupper = Chars ['A' .. 'Z']
dlower:: Regex
dlower = Chars ['a' .. 'z']
dspace :: Regex
dspace = Chars [' ']

dword = dupper :<>: Star dlower
dwords =  Plus (dword <> Plus dspace)