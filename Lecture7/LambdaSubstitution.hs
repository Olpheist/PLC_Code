type Name = String
data Expr = Const Int | Plus Expr Expr | Times Expr Expr 
        | Var Name| Lam Name Expr | App Expr Expr
  deriving (Eq, Show)

(@) :: Expr -> Expr -> Expr
(@) = App

-- \x -> x + x) 1
e1 = Lam "x" (Plus (Var "x") (Var "x")) @ Const 1

-- (\x x+x) (1 + 2)
e2 = Lam "x" (Plus (Var "x") (Var "x")) @ Plus (Const 1) (Const 2)

-- (\x -> (\y -> x + y) 1) 2
e3 =  Lam "x" (Lam "y" (Plus (Var "x") (Var "y")) @ Const 1) @ Const 2

-- explicit substitution
--             x       m       n       n[m/x]
substitute :: Name -> Expr -> Expr -> Expr
substitute x m (Const i) = Const i
substitute x m (Plus n1 n2) = Plus (substitute x m n1) (substitute x m n2)
substitute x m (Times n1 n2) = Times (substitute x m n1) (substitute x m n2)
substitute x m (App n1 n2) = App (substitute x m n1) (substitute x m n2)
substitute x m (Var y) 
    | x == y    = m
    | otherwise = Var y
substitute x m (Lam y n) 
    | x == y    = Lam y n
    | otherwise = Lam y (substitute x m n)

try :: Maybe a -> (a -> b) -> Maybe b -> Maybe b
try m f z = maybe z (Just . f) m

binaryCongGeneric :: (a -> Maybe a) -> (a -> a -> a) -> a -> a -> Maybe a
binaryCongGeneric step f m n = 
  try (step m)
    (\m' -> f m' n) -- apply f to the result of smallStep' m
    (try (step n) 
          (\n' -> f m n') 
          Nothing)

binaryCongCBN = binaryCongGeneric smallStepCBN

smallStepCBN :: Expr -> Maybe Expr
smallStepCBN (Const _)                   = Nothing
smallStepCBN (Var x)                     = Nothing
smallStepCBN (Lam _ _)                   = Nothing
smallStepCBN (Plus (Const i) (Const j))   = Just (Const (i + j))
smallStepCBN (Plus m n)                  = binaryCongCBN Plus m n

smallStepCBN (Times (Const i) (Const j)) = Just (Const (i * j))
smallStepCBN (Times m n)                 = binaryCongCBN Times m n





main = return ()