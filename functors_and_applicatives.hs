-- functions are also functors. Here's how fmap for functions is deinfed:

instance Functor ((->) r) where  
    fmap f g = (\x -> f (g x))  

add_3_then_multiply_by_2 = fmap (*2) (+3)
                         = (\x -> (*2) ((+3) x))  

-- NOTE! this is the same as composition: fn = (*2) . (+3)

-- compare this to a simpler functor: Maybe

instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing  

m = fmap (+1) (Just 1)
  = Just ((+1) 1)

-- this says: take the box (a "Just") that contains 1, and stick a transformer
-- on the box that 1 will be forced to go through on its way out of the box

-- for function functors (see above) it's something like:
-- there's an empty box (+3) that will permanently alter what's put in to it
-- when something is eventually put in to it, by adding 3 to whatever's put in.
-- So if a 7 is put in to the box, it becomes the thing known as "7+3".
-- Take that box and stick a transformer on it that the box
-- contents will be forced to go through on its way out of the box.
-- So on it's way out of the box, "7+3" goes through a transformation that turns
-- it in to "(7+3)*2"
-- THIS BOX METAPHOR IS EVEN MORE INTUITIVE IF YOU KEEP IN MIND HASKELL'S
-- LAZY EVALUATION! Think of these "transformations" as simply tacking on to
-- THE LEFT more expressions to evaluate:
-- 7 put in to the box becomes ((+3) 7); ((+3) 7) when leaving the box becomes
-- ( (*2) ((+3) 7) )

-- Applicatives are functors that contain functions.
-- If you want to apply the function in a functor to another functor, you have
-- to use the <*> operator

instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something  

two = Just (+1) <*> Just 1
    = fmap (+1) (Just 1)
    = Just ((+1) 1)
    = Just 2

three = Just (+) <*> Just 1 <*> Just 2
      = fmap (+) (Just 1) <*> Just 2
      = Just ((+) 1) <*> Just 2
      = fmap ((+) 1) (Just 2)
      = Just ( ((+) 1) 2 )
      = Just 3

-- Control.Applicative provides <$> which is just fmap as an infix.
-- So if you have a function that expects parameters that aren't necessarily
-- wrapped in functors, we can use that function to operate on several values
-- that are in functor contexts.

three = (+) <$> Just 1 <*> Just 2
      = fmap (+) (Just 1) <*> Just 2
      = ... yadda yadda see above

six = (\x y z -> x + y + z) <$> Just 1 <*> Just 2 <*> Just 3
    = Just ((\x y z -> x + y + z) 1) <*> Just 2 <*> Just 3
    = Just ( ( (\x y z -> x + y + z) 1 ) 2 ) <*> Just 3
    = etc...

-- *******
-- JUST SQUINT AND MELT AWAY THE <> AND THE $s *s AND Justs:
(\x y z -> x + y + z) <$> Just 1 <*> Just 2 <*> Just 3   == squint ==>  (\x y z -> x + y + z) 1 2 3

-- now, FUNCTIONS as applicatives

instance Applicative ((->) r) where  
    pure x = (\_ -> x)  
    f <*> g = \x -> f x (g x)  

-- let's get warmed up with fns as functors refresher
mult_fst_then_add_snd = fmap (+) (*2)
                      = (\x -> (+) ((*2) x))
mult_fst_then_add_snd 2 3
= (\x -> (+) ((*2) x)) 2 3
= (+) ((*2) 2) 3

fn = (+) <$> (+3) <*> (*100)
   = (\x -> (+) ((+3) x)) <*> (*100)
   = \y -> (\x -> (+) ((+3) x)) y ((*100) y) 
fn 5
= (+) ((+3) 5) ((*100) 5)
= 508

-- another cool example:
(\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5  
= [8.0,10.0,2.5]  

-- lifts elevate a normal function to work with two applicatives, hiding the applicative style.
-- So the fn above could instead be:
fn = liftA2 (+) (+3) (*100)
fn 5
> 508

-- check out this awesome sauce:
liftA2 (:) (Just 3) (Just [4])  
> Just [3,4]  

-- because (:) 3 [4] = [3,4]... i.e. 3:[4]
-- We can extend this like so:

sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA = foldr (liftA2 (:)) (pure [])  

sequenceA [Just 1, Just 2]
> Just [1,2]

sequenceA [Just 1, Nothing, Just 3]
> Nothing

-- in this case sequence makes a function that takes a parameter and feeds it
-- to all of the functions in the list. 

fn :: Int -> [Int]
fn = sequenceA [(+3),(+2),(+1)]

sequenceA [(+3),(+2),(+1)] 3  
> [6,5,4]  

sequenceA [(>4),(<10),odd] 7  
[True,True,True] 

sequenceA [getLine, getLine, getLine]  
heyh  
ho  
woo  
["heyh","ho","woo"]  

