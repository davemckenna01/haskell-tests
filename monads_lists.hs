instance Monad [] where  
    return x = [x]  
    xs >>= f = concat (map f xs)  
    fail _ = []

[1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
-- > [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-- the above chain of binds can be written as
listOfTuples :: [(Int,Char)]  
listOfTuples = do  
    n <- [1,2]  
    ch <- ['a','b']  
    return (n,ch)  

-- not valid Haskell
listOfTuples :: []
listOfTuples =
    [1,2]      >>= (\n -> 
    ['a', 'b'] >>= (\ch -> 
    return (n, ch) ))

-- again, won't compile, just illustrating how the first line ultimately evaluates
listOfTuples :: []
listOfTuples =
    [1,2] >>= (\n -> ['a', 'b'] >>= (\ch -> return (n, ch) ))
    concat (map (\n -> ['a', 'b'] >>= (\ch -> return (n, ch) )) [1,2] )
    concat (map (\n ->  concat (map (\ch -> return (n, ch) ) ['a', 'b']) ) [1,2] )
