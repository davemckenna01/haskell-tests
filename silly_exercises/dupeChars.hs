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

hasDupes' :: [Char] -> Map Char Bool -> Bool -> Maybe Bool
hasDupes' []     _     _     = Just False
hasDupes' _      _     True  = Just True
hasDupes' (x:xs) chars _
    | Map.lookup x chars == Nothing = hasDupes' xs (Map.insert x True chars) False
    | otherwise                     = hasDupes' xs  Map.empty                True

--chars :: Map Char Bool
--chars = Map.empty

dupes = hasDupes' "dave" Map.empty False


