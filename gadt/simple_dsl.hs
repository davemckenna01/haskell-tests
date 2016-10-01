-- Fooling around with the concepts here:
-- https://en.wikibooks.org/wiki/Haskell/GADT

-- data Expr = I Int        
--           | Add Expr Expr
--           | Mul Expr Expr

-- eval :: Expr -> Int
-- eval (I n) = n
-- eval (Add e1 e2) = eval e1 + eval e2
-- eval (Mul e1 e2) = eval e1 * eval e2

data Expr = I Int        
          | B Bool
          | Add Expr Expr
          | Eq Expr Expr

eval :: Expr -> Maybe (Either Int Bool)
eval (I n) = Just $ Left n
eval (B b) = Just $ Right b

eval (Add e1 e2) = 
    let e1' = eval e1
        e2' = eval e2
    in case (e1', e2') of 
        (Just (Left n1), Just (Left n2)) -> Just $ Left (n1 + n2)
        (_, _) -> Nothing

eval (Eq e1 e2) = 
    let e1' = eval e1
        e2' = eval e2
    in case (e1', e2') of
        (Just (Left n1), Just (Left n2)) -> Just $ Right (n1 == n2)
        (_, _) -> Nothing

-- *Main> eval $ Add (Add (I 1) (I 2)) (I 2)
-- Just (Left 5)
-- *Main> eval $ Eq (Add (Add (I 1) (I 2)) (I 2)) (I 5)
-- Just (Right True)
-- *Main> eval $ Add (Add (I 1) (I 2)) (B True)
-- Nothing