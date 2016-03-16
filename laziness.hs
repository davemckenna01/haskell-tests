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

{-
The following is my guess at how lazy evaluation might work.
It appears that Haskell's lazy evaluation works by
only ever evaluating a recursive list one level at a time. So it sees
ones'' and says, "ok, what's ones''? It's Cons 1 ones''. So I'll replace
ones'' with that. That's all I have to worry about right now."
It doesn't unwrap the function more than it needs to. Whatever function
gets called with ones'' next will deal with unwrapping it one more time
if it needs to.
-}

 {-
> take'' 1 ones''
> take'' 1 (Cons 1 ones'')
  take'' n (Cons x xs) = Cons x (take'' (n-1) xs)
                       = Cons 1 (take'' (1-1) ones'')
                       = Cons 1 (take'' 0 ones'')
                       = Cons 1 (take'' 0 (Cons 1 ones''))
                       = Cons 1 (Empty)
                       = Cons 1 Empty

> take'' 2 ones''
> take'' 2 (Cons 1 ones'')
  take'' n (Cons x xs) = Cons x (take'' (n-1) xs)
                       = Cons 1 (take'' (2-1) ones'')
                       = Cons 1 (take'' 1 ones'')
                       = Cons 1 (take'' 1 (Cons 1 ones''))
                       = Cons 1 (Cons 1 (take'' (1-1) ones''))
                       = Cons 1 (Cons 1 (take'' 0 ones''))
                       = Cons 1 (Cons 1 (take'' 0 (Cons 1 ones'')))
                       = Cons 1 (Cons 1 (Empty))
                       = Cons 1 (Cons 1 Empty)
 -}

--{-
map' f []     = []
map' f (x:xs) = (f x) : map' f xs

map'' f Empty       = Empty
map'' f (Cons x xs) = Cons (f x) (map'' f xs)


> map'' (+1) ones''
> map'' (+1) (Cons 1 ones'')
  map'' f    (Cons x xs)     = Cons (f x) (map'' f xs)
                             = Cons (+ 1 1) (map'' (+1) (Cons 1 ones''))


two_incrs = take'' 2 . map'' (+1)

> two_incrs ones''
> take'' 2 ( map'' (+1) ones'' )
> take'' 2 ( Cons (+ 1 1) (map'' (+1) (Cons 1 ones'')) )
  take'' n ( Cons x xs) = Cons x       ( take'' (n-1) xs )
                        = Cons (+ 1 1) ( take'' (2-1) (map'' (+1) (Cons 1 ones'')) )
                        = Cons (2)     ( take'' (1)   Cons (+ 1 1) (map'' (+1) (Cons 1 ones'')) )
                        = Cons (2)     ( Cons (+ 1 1) ( take'' (1-1) (map'' (+1) (Cons 1 ones'')) ) )
                        = Cons (2)     ( Cons (2)     ( take'' (0)   Cons (+ 1 1) (map'' (+1) (Cons 1 ones'')) ) )
                        = Cons (2)     ( Cons (2)     ( Empty ) )
-- -}




