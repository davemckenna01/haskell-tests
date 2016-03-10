reduce' f acc []     = acc
reduce' f acc (x:xs) = reduce' f (f acc x) xs

map' f []     = []
map' f (x:xs) = (f x) : map' f xs
