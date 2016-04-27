--import qualified Data.Maybe as M

class BadMonad m where  
    --bindIsh :: Num b => m a -> (a -> b) -> b  
    bindIsh :: m a -> (a -> b) -> b  

instance BadMonad Maybe where  
    bindIsh Nothing f   = 0
    bindIsh (Just x) f  = f x  


--------------------------------------

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
-- right there and "routine" returns Nothing