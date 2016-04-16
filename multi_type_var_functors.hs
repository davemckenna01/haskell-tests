--import Data.Functor

-- The Functor class:
class Functor f where
    fmap :: (a -> b) -> f a -> f b

data Either a b = Left a | Right b

-- fmap can ONLY be defined like this:
instance Functor (Either c) where   --STYLE 1
    fmap f (Right y) = Right (f y)  
    fmap f (Left x) = Left x

-- we can't define it as:

instance Functor (Either c) where   --STYLE 2 (Won't compile!)
    fmap f (Right y) = Right (y)  
    fmap f (Left x) = Left (f x)

-- because the type of fmap as defined by the Functor class is, for (Either c)'s:
fmap :: (a -> b) -> (Either c) a -> (Either c) b

-- So we know f takes a to b. If we define fmap as STYLE 2, we're saying
-- whatever type x is with (Left x), that's the type of c in (Either c).
-- And since we'd be taking x to b, that would mean the return value would be
-- (Either b) b, which it can't be b/c we defined it as (Either c) b!
