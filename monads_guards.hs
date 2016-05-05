class Monad m where  
    return :: a -> m a  
  
    (>>=) :: m a -> (a -> m b) -> m b  
  
    (>>) :: m a -> m b -> m b  
    x >> y = x >>= \_ -> y  
  
    fail :: String -> m a  
    fail msg = error msg 

instance Monad [] where  
    return x = [x]  
    xs >>= f = concat (map f xs)  
    fail _ = []

-- for reference
map' f []     = []
map' f (x:xs) = (f x) : map' f xs

-- guard (not to be confused with guards in fn definitions) can be used
-- to achieve filtering in monadic funtions, similar to filtering in
-- list comprehensions:
-- [ x | x <- [1..50], '7' `elem` show x ] 
-- > [7,17,27,37,47] 

guard :: (MonadPlus m) => Bool -> m ()  
guard True = return ()  
guard False = mzero  

sevensOnly :: [Int]  
sevensOnly = do  
    x <- [1..50]  
    guard ('7' `elem` show x)  
    return x 

-- with no sugar:
sevensOnly =
    [1..50] >>= (\x -> guard ('7' `elem` show x) >>= (\_ -> return x) ) 
    -- can be re-written as the following (b/c of >>'s definition)
    [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
    concat (map (\x -> guard ('7' `elem` show x) >> return x) [1..50])
    -- (map lambda [1..50]) is going to produce a list of one-element lists
    -- which will obvs then be concatted to one normal list.
    -- if ('7' `elem` show x) is False:
        concat ((\x -> guard ('7' `elem` show x) >> return x) 1 : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat (guard ('7' `elem` show 1) >> return 1 : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat (guard False >> return 1 : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat ([] >> return 1 : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat ([] >>= (\_ -> return 1) : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat (concat (map (\_ -> return 1) []) : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat (concat [] : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat ([] : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
    -- if ('7' `elem` show x) is True:
        concat ((\x -> guard ('7' `elem` show x) >> return x) 7 : map (\x -> guard ('7' `elem` show x) >> return x)  [8..50])
        concat (guard ('7' `elem` show 1) >> return 7 : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat (guard True >> return 7 : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat (return () >> return 7 : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat ([()] >> return 7 : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat ([()] >>= (\_ -> return 7) : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat (concat (map (\_ -> return 7) [()]) : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat (concat ((\_ -> return 7) () : map (\_ -> return 7) []) : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat (concat (return 7 : map (\_ -> return 7) []) : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat (concat ([7] : map (\_ -> return 7) []) : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat (concat ([7] : []) : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat (concat ([7]) : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
        concat ([7] : map (\x -> guard ('7' `elem` show x) >> return x)  [2..50])
    -- and ultimately...
        concat ([] : [] : [] : [] : [] : [] : [7] : [] : [] : ... [17] : [27] : [])
        concat ([[],[],[],[],[],[],[7],[17],[27]])
        [7,17,27]
