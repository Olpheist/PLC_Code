type Name = String
data Expr = Const Int | Plus Expr Expr | Times Expr Expr 
        | Var Name| Lam Name Expr | App Expr Expr | Subst Name Expr Expr --read "Subst x m n as n [n/m]"
  deriving (Eq, Show)

(@) :: Expr -> Expr -> Expr
(@) = App

-- \x -> x + x) 1
e1 = Lam "x" (Plus (Var "x") (Var "x")) @ Const 1

-- (\x x+x) (1 + 2)
e2 = Lam "x" (Plus (Var "x") (Var "x")) @ Plus (Const 1) (Const 2)

-- (\x -> (\y -> x + y) 1) 2
e3 =  Lam "x" (Lam "y" (Plus (Var "x") (Var "y")) @ Const 1) @ Const 2

-- (\f -> (\x -> f x) 1) (\y -> y + 1) 2
e4 = Lam "f" (Lam "x" (Var "f" @ Var "x")) @ (Lam "y" (Plus (Var "y") (Const 1))) @ Const 2

-- (\x -> \x -> x) 1 2
e5 = Lam "x" (Lam "x" (Var "x")) @ Const 1 @ Const 2

omega = (Lam "x" (Var "x" @ Var "x") @ (Lam "x" (Var "x" @ Var "x")))

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

untilNothing :: (a -> Maybe a) -> a -> [a]
untilNothing step x = x : steps (step x) where
  steps Nothing = []
  steps (Just y) = y : steps (step y)

prints :: Show a => [a] -> IO ()
prints = mapM_ print


binaryCong byName = binaryCongGeneric (smallStep byName)

--  map f l    (map (+1))
-- map l f     (map [1,2,3])

smallStep :: Bool -> Expr -> Maybe Expr
smallStep _      (Const _)                   = Nothing
smallStep _      (Var _)                     = Nothing
smallStep _      (Lam _ _)                   = Nothing
smallStep _      (Plus (Const i) (Const j))  = Just (Const (i + j))
smallStep byName (Plus m n)                  = binaryCong byName Plus m n
smallStep _      (Times (Const i) (Const j)) = Just (Const (i * j))
smallStep byName (Times m n)                 = binaryCong byName Times m n
smallStep byName  (App (Lam x n) m)
    | byName = Just (Subst x m n )
    | otherwise = try (smallStep byName n) (App (Lam x m))
                      (Just (Subst x n m))
smallStep byName (App m n) = try (smallStep byName m)
                                    (\m' -> App m' n) Nothing
smallStep byName (Subst x m (Const i)) = Just (Const i)
smallStep byName (Subst var1 m (Var var2))
    | var1 == var2   = Just m
    | otherwise      = Just (Var var2)
smallStep byName (Subst x m (Plus n1 n2))  = Just (Plus (Subst x m n1) (Subst x m n2))
smallStep byName (Subst x m (Times n1 n2)) = Just (Times (Subst x m n1) (Subst x m n2))
smallStep byName (Subst x m (App n1 n2))   = Just (App (Subst x m n1) (Subst x m n2))
smallStep byName (Subst var1 m (Lam var2 n))
    | var1 == var2 = Just (Lam var2 n)
    | otherwise    = Just (Lam var2 (Subst var1 m n))
smallStep byName (Subst x m n) = try (smallStep byName n)
                                     (Subst x m ) Nothing
  
smallSteps :: Bool -> Expr -> [Expr]
smallSteps byName = untilNothing (smallStep byName)





binaryCongCBV = binaryCongGeneric smallStepCBV

--CBV is call by value
smallStepCBV :: Expr -> Maybe Expr
smallStepCBV (Const _)                   = Nothing
smallStepCBV (Var x)                     = Nothing
smallStepCBV (Lam _ _)                   = Nothing
smallStepCBV (Plus (Const i) (Const j))  = Just (Const (i + j))
smallStepCBV (Plus m n)                  = binaryCongCBV Plus m n
smallStepCBV (Times (Const i) (Const j)) = Just (Const (i * j))
smallStepCBV (Times m n)                 = binaryCongCBV Times m n
smallStepCBV (App (Lam x n) m)           = try (smallStepCBV n) 
                                               (App (Lam x m)) 
                                               (Just (substitute x m n))
smallStepCBV (App m n)                   = binaryCongCBV App m n

--Example from 2025.04.10 lecture
--Lam "x" ((Lam "x" (( Times (Const 2) (Var "x")) @ Plus (Const 3 (Var "x"))) @ 5
{- substitute "x" (Const 5) (Lam "x" (( Times (Const 2) (Var "x")) @ Plus (Const 3 (Var "x")))) 
      substitute "x" (Const 4) ((Lam "x" ((Times (Const 2) (Var "x")) )
      substitute "x" (Const 5) ((PLus ((COnst 3 (Var "x")))
          substitute "x" (Const 5) (Const 3) ---> Const 3
          subsitute "x" (Const 5) (Var "x") ---> Const 5
-}

main = return ()