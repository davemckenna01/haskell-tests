-- a Writer is basically a value with an attached monoid. The monoid can
-- act as a log of stuff that happend to get the value where it is now. Eg.

addTax :: Num -> (Num, String)
addTax x = (x * 1.13, "Added Tax.")

(10.00, "Money submitted.") >>= addTax

= (11.30, "Money submitted.AddedTax.")

-- Let's see how it works with do notation:

-- note when you see (Int, String) it should really be Writer w a or something
-- like that. See: http://learnyouahaskell.com/for-a-few-monads-more#writer
instance (Monoid w) => Monad (Int, String) where  
    return x = (x, mempty)  
    (x,v) >>= f = let (y, v') = f x in (y, v `mappend` v') 


multWithLog :: (Int, String)
multWithLog = do
    a <- (1, "Got number 1.")
    b <- (2, "Got number 2.")
    return (a * b)

multWithLog =
(1, "Got number 1.") >>= (\a -> (2, "Got number 2.") >>= (\b -> return (a * b)))
let (y, v') =      (\a -> (2, "Got number 2.") >>= (\b -> return (a * b))) 1      in (y, "Got number 1." `mappend` v')
let (y, v') =      (2, "Got number 2.") >>= (\b -> return (1 * b))                in (y, "Got number 1." `mappend` v')
let (y, v') =      let (y, v') = (\b -> return (1 * b)) 2 in (y, "Got number 2." `mappend` v')              in (y, "Got number 1." `mappend` v')
let (y, v') =      let (y, v') = return (1 * 2) in (y, "Got number 2." `mappend` v')                        in (y, "Got number 1." `mappend` v')
let (y, v') =      let (y, v') = return 3 in (y, "Got number 2." `mappend` v')                              in (y, "Got number 1." `mappend` v')
let (y, v') =      let (y, v') = (3, "") in (y, "Got number 2." `mappend` v')                               in (y, "Got number 1." `mappend` v')
let (y, v') =      (3, "Got number 2." `mappend` "")                                                        in (y, "Got number 1." `mappend` v')
let (y, v') =      (3, "Got number 2.")                                           in (y, "Got number 1." `mappend` v')
(3, "Got number 1." `mappend` "Got number 2.")
(3, "Got number 1.Got number 2.")

-- You might be tempted to think do notation is allowing imperative style, but
-- it's not. See the >>= expansion above. It's just imperative looking syntax.
-- It still just represents an expression. As further proof, look at how the
-- final value of the expression is constructed. It works backwards from the 
-- return statement, first calculating a * b, then appending ""
-- to "Got number 2.", and then appending that result to "Got number 1."

-- NOTE!!! LYAH teaches writers in a different way than Haskell now implements
-- them. Read: http://stackoverflow.com/questions/11684321/how-to-play-with-control-monad-writer-in-haskell
