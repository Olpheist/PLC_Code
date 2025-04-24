type Name = String

data Expr = Const Int | Plus Expr Expr | Times Expr Expr 
        | Var Name| Lam Name Expr | App Expr Expr | Store Expr | Recall
  deriving (Eq, Show)

(@) :: Expr -> Expr -> Expr
(@) = App

e1 = Plus (Store (Const 2)) Recall

e2 = Lam "x" (Plus Recall (Var "x")) @ Store (Const 1)
e2' = Lam "x" (Plus (Var "x") Recall) @ Store (Const 1)

e3 = Lam "x" (Plus Recall (Var "x")) @ Store (Plus (Const 1) Recall)

e4 = Store (Lam "x" (Recall @ Var "x")) @ Const 1

type State = (Expr, Expr) --(Program, Store)

smallStep :: Bool -> Bool -> State -> Maybe State
smallStep ltr cbv (Const _, ) = Nothing
smallStep ltr cbv (Plus (Const i) (Const j), acc) = Just (Const i + j, acc)
smallStep ltr cbv (Plus x y, acc)
  | ltr = case (smallStep ltr cbv (x, acc), smallStep ltr cbv (y, acc)) of
      (Just (x', acc'), _) -> Just (Plus x' y, acc')
      (_, Just (y', acc')) -> Just (Plus x y', acc')
      _ -> Nothing
  | otherwise = case (smallStep ltr cbv (x, acc), smallStep ltr cbv (y, acc)) of
      (_, Just (y', acc')) -> Just (Plus x y', acc')
      (Just (x', acc'), _) -> Just (Plus x' y, acc')
      _ -> Nothing
smallStep ltr cbv (Recall, acc) = Just (acc, acc)
      

bigStep :: Bool -> Bool -> Expr -> Expr -> Maybe State
bigStep ltr cbv (Const i ) acc = Just (Const i, acc)
bigStep ltr cbv (Plus i j) acc
  case bigStep ltr cbv n1 acc of 
    Nothing -> Nothing
    Just (Const i, acc) ->
      case bigStep ltr cbv n2 acc' of
        Nothing -> Nothing
        Just (Const i, acc'') -> Just (Const i + j, acc'')
        _ -> Nothing
    _ -> Nothing
  where (n1, n2) | ltr = (m, n)
                 | otherwise = (n, m)
bigStep ltr cbv Recall acc = Just (acc, acc)
bigStep ltr cbv (Store exp) acc = 
  case bigStep ltr cbv m acc of
    Just (v, acc')
      | cbv = Just (v, v)
      | otherwise = Just (v, exp)
    _ -> Nothing

main = return ()