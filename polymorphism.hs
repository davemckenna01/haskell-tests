{-
Polymorphism in Haskell has a different interface than most OOP implementations.
rather than an instance of Product being p.mappend(p) and defining the
behaviour of mappend on something like Product.mappend = x * y, Haskell
knows that when mappend is called with Product args, it does x*y, and when it's
called with Sum args, it does x+y
-}

instance Num a => Monoid (Product a) where
    mempty = Product 1
    Product x `mappend` Product y = Product (x * y)

instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)
