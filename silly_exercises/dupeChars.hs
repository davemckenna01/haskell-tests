import Data.Map (Map)
import qualified Data.Map as Map

-- O(n^2)
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


-- O(log n) ?

chars :: Map Char Bool
chars = Map.empty

hasDupes' :: [Char] -> Maybe Bool
hasDupes' [] = Just False
hasDupes' (x:xs) =
    if Map.lookup x chars == Nothing
        then Nothing
    else
        Map.insert x True chars;


