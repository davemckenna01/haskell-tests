reduceL f acc []     = acc
reduceL f acc (x:xs) = reduceL f (f acc x) xs

reduceR f acc []     = acc
reduceR f acc (x:xs) = f x (reduceR f acc xs)

-- > print $ reduceL (-) 0 [1,2,3]
-- > -6

-- > print $ reduceR (-) 0 [1,2,3]
-- > 2

data List a = Empty | Cons a (List a) deriving (Show)

reduceL' f acc Empty       = acc
reduceL' f acc (Cons x xs) = reduceL' f (f acc x) xs

reduceR' f acc Empty       = acc
reduceR' f acc (Cons x xs) = f x (reduceR' f acc xs)

-- > print $ reduceL' (-) 0 (Cons 1 (Cons 2 (Cons 3 Empty)))
-- > -6

-- > print $ reduceR' (-) 0 (Cons 1 (Cons 2 (Cons 3 Empty)))
-- > 2


{-
when you reduceR' (-) 0 (Cons 1 (Cons 2 Empty))

reduceR' f acc Empty       = acc
reduceR' f acc (Cons x xs) = f x (reduceR' f acc xs)
                           = (-) 1 (reduceR' (-) 0 (Cons 2 Empty))
                           = (-) 1 ((-) 2 (reduceR' (-) 0 Empty))
                           = (-) 1 ((-) 2 (0))
                           = (-) 1 (2)
                           = -1

One way to understand (reduceR f a) is as a function that replaces
all occurrences of Cons in a list by f, and all occurrences
of Empty by a.

In the example above:

(Cons 1 (Cons 2 Empty))
foldr - 0
(- 1 (- 2 0))
(- 1 2)
= -1

(Cons 1 (Cons 2 (Cons 3 Empty)))
foldr - 0
(- 1 (- 2 (- 3 0)))
(- 1 (- 2 3))
(- 1 -1)
= 2


And also, awesomely, reduceR (:) [] copies a list:

> print $ reduceR' Cons Empty (Cons 1 (Cons 2 (Cons 3 Empty)))
> Cons 1 (Cons 2 (Cons 3 Empty))

> print $ reduceR (:) [] [1,2,3]
> [1,2,3]

-}
