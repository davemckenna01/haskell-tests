import qualified Data.Maybe as M

-- BIND. The bind operator takes a monad, a fn that takes a regular value and
-- returns a monad, and then returns a monad
(>>=) :: Monad m => m a -> (a -> m b) -> m b

foo :: Maybe
foo = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))

-- can be thought of as:

foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

-- ... can be thought of as:

foo = 
    x = 3
    y = "!"
    Just (show x ++ y)

-- And this is basically DO-notation! But DO-notation is obvs better.

foo = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y)  

-- The whole idea of do notation requiring that values are only temporarily
-- extracted to non-monad land, by requiring the block to return a monad, is
-- because we want to PRESERVE THE CONTEXT of these values. In the Maybe monad's
-- case, we want to preserve the ability to make the whole chain FAIL, (i.e. 
-- become a Nothing) if even one of the values is a nothing. If we were to
-- simply return a vanilla value from the block, we'd never know if anything
-- failed.

--------------------------------------------------------------------------------
-- The following illustrates how putting a Nothing on it's own line in a do
-- block "throws a wrench"

-- NOTE! This Pole stuff below won't make sense without looking at the example
-- here: http://learnyouahaskell.com/a-fistful-of-monads#walk-the-line

routine :: Maybe Pole  
routine = do  
    start <- return (0,0)  
    first <- landLeft 2 start  
    Nothing  
    second <- landRight 2 first  
    landLeft 1 second  

-- the above but using binds instead of sugar

routine :: Maybe Pole  
routine =
    return (0,0)      >>= (\start -> 
    landLeft 2 start  >>= (\first -> 
    -- b/c there's no value to extract like with a Just, we don't name the arg
    Nothing           >>= (\_ ->
    landRight 2 first >>= (\second -> 
    landLeft 1 second ))))

-- the key thing to realize is that >>= is defined such that if the left arg
-- is ever a Nothing, >>= returns a Nothing. So we short circuit the evaluation
-- right there and "routine" returns Nothing:

instance Monad Maybe where  
    return x = Just x  
    Nothing >>= f = Nothing  
    Just x >>= f  = f x  
    fail _ = Nothing  


-- here is the above fully evaluated
routine :: Maybe Pole  
routine =
    return (0,0) >>= (\start -> landLeft 2 start  >>= (\first -> Nothing >>= (\_ -> landRight 2 first >>= (\second -> landLeft 1 second ))))
    (\start -> landLeft 2 start  >>= (\first -> Nothing >>= (\_ -> landRight 2 first >>= (\second -> landLeft 1 second )))) (0,0)
    landLeft 2 (0,0) >>= (\first -> Nothing >>= (\_ -> landRight 2 first >>= (\second -> landLeft 1 second ))) 
    Just (2,0) >>= (\first -> Nothing >>= (\_ -> landRight 2 first >>= (\second -> landLeft 1 second ))) 
    (\first -> Nothing >>= (\_ -> landRight 2 first >>= (\second -> landLeft 1 second ))) (2,0)
    Nothing >>= (\_ -> landRight 2 first >>= (\second -> landLeft 1 second ))
    Nothing



--------------------------------------------------------------------------------