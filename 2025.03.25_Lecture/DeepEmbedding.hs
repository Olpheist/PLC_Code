import Prelude hiding ((<>), (<|>), words)

import Data.List ((\\))

-- Regex means regular expression
type Regex = [String]


data DRegex = AnyChars | Chars [Char] | NotChars [Char] |
    DRegex:|: DRegex | None | DRegex :<>: DRegex | Empty | Question DRegex |
    Plus DRegex | Star DRegex
        deriving (Show)

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

crossWith :: (a -> b -> c) -> [a] -> [b] -> [c]
crossWith f xs ys = concatMap withY ys where
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

showR :: Regex -> String
showR AnyChars      = "."
showR None          = "0"
showR Empty         = "e"
showR (Chars cs)    = "[" ++ cs ++ "]"
showR (NotChars cs) = "[^" ++ cs ++ "]"
showR (Question r)  = (showR) ++ "?"
showR (Plus r)      = (showR) ++ "+"
showR (Star r)      = (showR) ++ "*"
showR (r :|: s)     = (showR r) ++ "|" ++ (showR s)
showR (r :<>: s)    = (showR r) ++ (showR s)
-- regex101.com to explain regular expressions

enumerate :: Regex -> String
enumerate AnyChars      = map (\ c -> [c]) alphabet
enumerate None          = []
enumerate Empty         = [""]
enumerate (Chars cs)    = map (\ c -> [c]) cs
enumerate (NotChars cs) = map (\ c -> [c]) (alphabet \\ cs)
enumerate (Question r)  = enumerate (Empty :|: r)
enumerate (Plus r)      = enumerate (r :<>: (Star r))
enumerate (Star r)      = enumerate (Question (Plus r))
enumerate (r :|: s)     = (enumerate r) ++ (enumerate s)
enumerate (r :<>: s)    = crossWith (++) (enumerate r) (enumerate s)
--take 100 $ enumerate words

recognizes ::  Regex -> String -> Bool
recognizes AnyChars [c]   = c `elem`` alphabet
recognizes AnyChars s     = False -- for example: s = "Iowa"
recognizes (Chars cs) [c] = c `elem`` cs
recognizes (Chars cs) s   = False
recognizes None s         = False
recognizes Empty s        = s == ""
recognizes (NotChars cs)  = recognizes (Chars (alphabet \\ cs)) s
recognizes (Question r) s = recognizes (Empty :|: r) s
recognizes (Plus r) s     = recognizes (r :<>: (Star r)) s
recognizes (Star r) s     = recognizes (Questino (Plus r)) s
recognizes (r :|: s) s    =
recognizes (r :<>: s) s   = 

--We want:
--recognizes R S = S 'elem' (enumerate R)
-- filter (recognizes R) allStrings = enumerate R