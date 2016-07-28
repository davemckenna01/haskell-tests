--IO a really means the following:
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

{-
State# is an "unboxed type" which is an optimization that makes it refer
to a primitive value (think C values like int and float) rather than a thunk,
which is what most "normal" Haskell values are (right??). They're put on the
stack instead of heap, which is faster I guess?

(# a, b #) is an "unboxed tuple" serving a similar purpose.
-}

--Simplified IO for my little puny mind:
newtype IO a = IO (Int -> (Int, a))


getChar :: IO Char


bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO (\ s -> case m s of (# new_s, a #) -> unIO (k a) new_s)
--simplified bindIO for my puny mind:
bindIO (IO m) k = IO (\ s -> case m s of (new_s, a) -> unIO (k a) new_s)


unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
--simplified unIO for my puny mind:
unIO :: IO a -> (Int -> (Int, a))
unIO (IO a) = a


main :: IO ()
main = getChar >>= (\ch1 -> return ())
     = getChar bindIO (\ch1 -> return ())
     = bindIO getChar (\ch1 -> return ())






http://stackoverflow.com/questions/9244538/what-are-the-definitions-for-and-return-for-the-io-monad
https://hackage.haskell.org/package/base-4.9.0.0/docs/src/GHC.IO.html
- "The IO monad is just an instance of the State-Transformer monad"
https://hackage.haskell.org/package/base-4.9.0.0/docs/src/GHC.ST.html

the State# is simply used to preserve statement order:
"GHC uses impure primitives to implement these IO operations; the State# RealWorld
"values" are only to stop the compiler reordering statements by introducing data
dependencies from one statement to the next."
... it's not actually the entire state of the world!
The IO monad is not a state monad. The state monad looks very similar to the type GHC uses for IO. Thus the confusion.
Notice the reversed order of values in state vs io return tuple.

(from http://stackoverflow.com/questions/9244538/what-are-the-definitions-for-and-return-for-the-io-monad)












