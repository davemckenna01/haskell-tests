--https://en.wikipedia.org/wiki/Referential_transparency#Contrast_to_imperative_programming

y = 0

incr x =
    if y == 0 then x + 2
    else           x + 1

incr 1

-- is ALWAYS the exact same as:

1 + 1

-- b/c y can never change

-- Haskell can do this because there's no mutable state allowed. Modifying
-- y after it was declared would be an example of mutable state