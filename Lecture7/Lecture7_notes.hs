data Expr = Const Int | Plus Expr Expr | Times Expr Expr | Negate Expr | 
    Pair Expr Expr | Fst Expr | Snd Expr
  deriving (Show)

-- Fst: (Int, Int) -> Int

-- Fst (Const 5) would be rejected by the type checker, because Const 5 is not a pair.

isValue :: Expr -> Bool
isValue (Const _)  = True
isValue (Pair m n) = isValue m && isValue n
isValue _          = False

asConst:: Expr -> Int
asConst (Const i) = i
asConst e         = error ("Expected " ++ show e ++ "to be an integer constant")

asPair:: Expr -> (Expr, Expr)
asPair (Pair i j) = (i, j)
asPair e          = error ("Expected " ++ show e ++ "to be a pair")

bigStep :: Expr -> Expr
bigStep (Const i)   = Const i
bigStep (Plus m n)  = Const (asConst (bigStep m) + asConst (bigStep n))
bigStep (Times m n) = Const (asConst (bigStep m) * asConst (bigStep n))
bigStep (Negate m)  = Const (- asConst (bigStep m))
bigStep (Pair m n)  = Pair (bigStep m) (bigStep n)
bigStep (Fst m)     = fst (asPair (bigStep m))
bigStep (Snd m)     = snd (asPair (bigStep m))

data Value = VInt Int | VPair Value Value
  deriving (Show)

intv:: Value -> Int
intv (VInt i) = i
intv e         = error ("Expected " ++ show e ++ "to be a value")

pairv:: Value -> (Value, Value)
pairv (VPair i j) = (i, j)
pairv e          = error ("Expected " ++ show e ++ "to be a value pair")

bigStep' :: Expr -> Value
--Haskell type checker will guarantee that the result of bigStep' is a value.
-- The type checker will not allow us to return a non-value.
bigStep' (Const i)   = VInt i
bigStep' (Plus m n)  = VInt (intv (bigStep' m) + intv (bigStep' n))
bigStep' (Times m n) = VInt (intv (bigStep' m) * intv (bigStep' n))
bigStep' (Negate m)  = VInt (- intv (bigStep' m))
bigStep' (Pair m n)  = VPair (bigStep' m) (bigStep' n)
bigStep' (Fst m)     = fst (pairv (bigStep' m))
bigStep' (Snd m)     = snd (pairv (bigStep' m))

smallStep :: Expr -> Expr
smallStep (Const i)  = error "no step"
smallStep (Plus (Const i) (Const j)) = Const (i + j)
smallStep (Plus m n )
    | isValue m = Plus m (smallStep n)
    | otherwise = Plus (smallStep m) n
smallStep (Times (Const i) (Const j)) = Const (i * j)
smallStep (Times m n )
    | isValue m = Times m (smallStep n)
    | otherwise = Times (smallStep m) n
smallStep (Negate (Const i)) = Const (- i)
smallStep (Negate m) = Negate (smallStep m)
smallStep (Pair m n)
    | isValue m = Pair m (smallStep n)
    | otherwise = Pair (smallStep m) n
smallStep (Fst (Pair m n ))
    | isValue m = m  -- if m is a value, return it directly
    | otherwise = Fst (smallStep m)  -- otherwise, keep stepping on m
smallStep (Fst m) = Fst (smallStep m)
smallStep (Snd (Pair m n )) = n
smallStep (Snd m) = Snd (smallStep m)
smallSteps :: Expr -> [Expr]
smallSteps m
  | isValue m = [m]
  | otherwise = m : smallSteps (smallStep m)

try :: Maybe a -> (a -> b) -> Maybe b -> Maybe b
try m f z = maybe z (Just . f) m

binaryCong :: (Expr -> Expr -> Expr) -> Expr -> Expr -> Maybe Expr
binaryCong f m n = try (smallStep' m)
                       (\m' -> f m' n) -- apply f to the result of smallStep' m
                       (try (smallStep' n) 
                            (\n' -> f m n') 
                            Nothing)

unaryCong :: (Expr -> Expr) -> Expr -> Maybe Expr
unaryCong f m = try (smallStep' m) f Nothing
-- smallStep' is a version of smallStep that returns a Maybe Expr

smallStep' :: Expr -> Maybe Expr
smallStep' (Const i)  = Nothing

smallStep' (Plus (Const i) (Const j)) = Just (Const (i + j))
smallStep' (Plus m n)                 = binaryCong Plus m n

smallStep' (Times (Const i) (Const j)) = Just (Const (i * j))
smallStep' (Times m n)                 = binaryCong Times m n

smallStep' (Negate (Const i)) = Just (Const (negate i))
smallStep' (Negate m)         = unaryCong Negate m

smallStep' (Pair m n)       = binaryCong Pair m n
smallStep' (Fst (Pair m n)) = Just m
smallStep' (Fst m)          = unaryCong Fst m
smallStep' (Snd (Pair m n)) = Just n
smallStep' (Snd m)          = unaryCong Snd m

smallSteps' :: Expr -> [Expr]
smallSteps' m = 
  case smallStep' m of
    Nothing -> [m]  -- if no more steps, return the value
    Just m' -> m : smallSteps' m'  -- otherwise, keep stepping on m'

main = return ()