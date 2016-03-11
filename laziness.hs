ones = 1 : ones

data List a = Empty | Cons a (List a) deriving (Show)

ones' = (Cons 1 (Cons 1 (Cons 1 Empty)))

ones'' = Cons 1 ones''

take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  

take'' n _  
     | n <= 0   = Empty
take'' _ Empty  = Empty  
take'' n (Cons x xs) = Cons x (take'' (n-1) xs)


> take'' 1 ones''
take'' 1 (Cons 1 ones'')
take'' n (Cons x xs) = Cons x (take'' (n-1) xs)
                     = Cons 1 (take'' (1-1) ones'')
                     = Cons 1 (take'' 0 ones'')
                     = Cons 1 (take'' 0 (Cons 1 ones''))
                     = Cons 1 (Empty)
                     = Cons 1 Empty

> take'' 2 ones''
take'' 2 (Cons 1 ones'')
take'' n (Cons x xs) = Cons x (take'' (n-1) xs)
                     = Cons 1 (take'' (2-1) ones'')
                     = Cons 1 (take'' 1 ones'')
                     = Cons 1 (take'' 1 (Cons 1 ones''))
                     = Cons 1 (Cons 1 (take'' (1-1) ones''))
                     = Cons 1 (Cons 1 (take'' 0 ones''))
                     = Cons 1 (Cons 1 (take'' 0 (Cons 1 ones'')))
                     = Cons 1 (Cons 1 (Empty))
                     = Cons 1 (Cons 1 Empty)


