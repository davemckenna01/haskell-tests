newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)
  
fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []


l1 = DiffList ("dave"++)
l2 = DiffList ("iscool"++)


instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))  


l3 = fromDiffList $ l1 `mappend` l2 -- > "daveiscool"