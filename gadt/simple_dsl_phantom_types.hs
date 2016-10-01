-- data Expr = I Int        
--           | B Bool
--           | Add Expr Expr
--           | Eq Expr Expr

-- eval :: Expr -> Maybe (Either Int Bool)
-- eval (I n) = Just $ Left n
-- eval (B b) = Just $ Right b

-- eval (Add e1 e2) = 
--     let e1' = eval e1
--         e2' = eval e2
--     in case (e1', e2') of 
--         (Just (Left n1), Just (Left n2)) -> Just $ Left (n1 + n2)
--         (_, _) -> Nothing

data Expr a = I Int
            -- | B Bool
            | Add (Expr a) (Expr a)
            -- | Eq  (Expr a) (Expr a)


-- These are called "smart constructors"
add :: Expr Int -> Expr Int -> Expr Int
add = Add
i :: Int  -> Expr Int
i = I
-- b :: Bool -> Expr Bool
-- b = B

-- NOTE: we can't write an eval using phantom types... how would the compiler
-- know that encountering the constructor I means that a = Int? We need GADTs.
eval :: Expr a -> a
eval (I n) = n 