-- a polymorphic function. "a" is a placeholder for any type, and is called a
-- "type variable". Type var in signature = a polymorphic fn
len :: [a] -> Int
len [] = 0
len (x:xs) = length xs + 1


-- "Char" is a "concrete type"
dave :: [Char]
dave = "Dave"


-- repeated type variables always represent the same type
head' :: [a] -> a
head' (x:xs) = x


-- sometimes you need to add constraints to your type variables
-- b/c sum' :: [a] -> a won't work, b/c summing non-numbers makes no sense
sum' :: Num a => [a] -> a
-- the double arrow indicates a constraint on the type variable
sum' [] = 0
sum' (x:xs) = x + sum xs

-- Num is a "type class". It represents all types which can do something.
-- In this case, all types which can do numeric things.

-- Another example:
-- show' :: Show a => a -> [Char]
-- show is kinda like Haskell's toString. But only some types can be
-- represented as strings. For example fns can not. So we constrain that
-- only those that are part of the Show type class are allowable args.

-- can also have multiple type class constraints:
showSum :: (Num a, Show a) => [a] -> [Char]
showSum xs = show (sum xs)
-- so a must be a num to allow the sum, but also showable to allow the show