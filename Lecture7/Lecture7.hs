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

main = return ()