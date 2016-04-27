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

bindIsh :: Monad m => m a -> (a -> b) -> b
bindIsh (Just x) f = f x