{-
Reverse Polish Notation

http://learnyouahaskell.com/functionally-solving-problems#reverse-polish-notation-calculator

Fn for evaluating a RPN expression.

  10 - (4 + 3) * 2
= 10 4 3 + 2 * -

eval "10 4 3 + 2 * -"
> -4

-}

eval :: (Read a, Num a) => String -> a

eval = head . foldl operation [] . words
    where operation (x:y:ys) "*" = (x * y):ys
          operation (x:y:ys) "+" = (x + y):ys
          operation (x:y:ys) "-" = (x - y):ys
          operation xs        a  = read a:xs

