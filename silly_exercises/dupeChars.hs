hasDupes :: [Char] -> Bool
hasDupes [] = False
hasDupes (x:xs) = 
    if elem x xs then True
    else hasDupes xs


-- this is just "elem"
contains1 :: Eq x => x -> [x] -> Bool
contains1 x [] = False
contains1 x (y:ys) 
    | x == y    = True
    | otherwise = contains1 x ys


