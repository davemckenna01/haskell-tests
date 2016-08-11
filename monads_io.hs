{-
The reason for making IO monadic has nothing to do with it lessening impurity
and everything to do with ensuring order of IO operations. 

But, why is IO getting out of order a risk? Because the compiler assumes
everything is pure and could possibly rearrange expressions UNLESS it sees
a data dependency.

(https://en.wikipedia.org/wiki/Data_dependency)

This is your bible:
https://wiki.haskell.org/IO_inside

It says:
"The Haskell language doesn't provide any way to express order of evaluation... except for data dependencies"

This means the IO monad simply ensures there are data dependencies among a sequence of 
IO operations. That is, if T1, T2, T3 are operations, IO constructs a sequence
such that T3 takes a result of T2 as a parameter, and T2 takes a result of T1
as a parameter. So the only way the compiler can interpret that is that T1 must
be run first, followed by T2 then T3.

There's no getting around the fact that IO will be impure, b/c it interacts with the outside world
aka the OS. getChar will not return the same value every time it's run, so it's impure. It
will also have a side effect of shortening the length of a kernel buffer by consuming a char.
putChar will return the same thing every time, a (), but it has a side effect of adding to
some kernel buffer, so it's impure in that way. BUT WHO CARES! No usefull program
can ever be pure. The best you can hope for is that a lot of your program is, so that you
get as much of the benefits of purity (compiler optimizations, laziness, etc.)


Q: Why would the compiler reorder? Is it simply because no order is specified 
so it arbitrarily chooses order? Or does it try and run stuff in parallel? I
feel like purity buys you easy concurrency. Laziness buys you stuff too, and it
requires purity I think. Referential transparency is basically another word for
purity, isn't it?


Side note:
Good answer here on "why do we need monads" (not particular to IO):
http://stackoverflow.com/questions/28139259/why-do-we-need-monads

-}


--IO a really means the following:
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

-- so when getChar says it's an IO a, it's really a IO (State# RealWorld -> (# State# RealWorld, a #))

{-
State# is an "unboxed type" which is an optimization that makes it refer
to a primitive value (think C values like int and float) rather than a thunk,
which is what most "normal" Haskell values are (right??). They're put on the
stack instead of heap, which is faster I guess?

(# a, b #) is an "unboxed tuple" serving a similar purpose.
-}

--Simplified IO for my little puny mind:
newtype IO a = IO (Int -> (Int, a))

--https://github.com/ghc/ghc/blob/master/libraries/base/System/IO.hs
getChar :: IO Char
getChar =  hGetChar stdin

--hGetChar is a frightening rabbit-hole that you will never return from. Find
--it here:
--https://github.com/ghc/ghc/blob/master/libraries/base/GHC/IO/Handle/Text.hs
--suffice it to say that getChar blocks the thread and waits till the stdin
--file handle (this is not right usage of term) gives it a "z" (or whatever)

--simplified getChar for my puny mind:
getChar = IO \world -> (world, Char)

-- Definition of the IO monad's >>= :

>>= = bindIO

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO m) k = IO (\ s -> case m s of (# new_s, a #) -> unIO (k a) new_s)
--simplified bindIO for my puny mind:
bindIO (IO m) k = IO (\ s -> case m s of (new_s, a) -> unIO (k a) new_s)


unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
--simplified unIO for my puny mind:
unIO :: IO a -> (Int -> (Int, a))
unIO (IO a) = a


-- a program that reads 2 chars and then exits
main :: IO ()
main = getChar >>= (\ch1 -> getChar >>= (\ch2 -> return ()))
     = getChar bindIO (\ch1 -> getChar bindIO (\ch2 -> return ()))
     = bindIO getChar (\ch1 -> bindIO getChar (\ch2 -> return ()))
     -- "||" is my silly way of representing the "impure primitive" outside world
     -- interaction that getChar performs
     = bindIO (IO \world -> (world, ||)) (\ch1 -> bindIO (IO \world -> (world, ||)) ( \ch2 -> IO \world -> (world, ()) ) )
     IO (\ s -> case (\world -> (world, ||)) s of (new_s, a) -> unIO (       (\ch1 -> bindIO (IO \world -> (world, ||)) ( \ch2 -> IO \world -> (world, ()) ) )             a) new_s)
     IO (\ s -> case (\world -> (world, ||)) s of (new_s, a) -> unIO (       (\ch1 -> IO (\ s -> case (\world -> (world, ||)) s of (new_s, a) -> unIO (( \ch2 -> IO \world -> (world, ()) ) a) new_s) )             a) new_s)


     -- Fully expanded now. Time to reduce.

     --now imagine C-land giving main a kick-off world value of 123 (after
     --discarding the IO constructor part at the beginning)
     -- TODO: find the code where C-land does this
     --UPDATE: actually, rather than maintaining the word "world" throughout,
     --I think the compiler creates brand new variable names like
     --(eta_ag6, then new_s_alV, then new_s1_alJ) in place of "world"
     --that serve to denote the changing of state from one statement to the next
     --which tells the compiler there are data dependencies and thus to not
     --optimize by rearranging: http://blog.ezyang.com/2011/05/unraveling-the-mystery-of-the-io-monad/
     --so something like:
     --IO (\ eta_ag6 -> case (\world -> (world, ||)) eta_ag6 of (new_s_alV, a) -> etc...


     (\ s -> case (\world -> (world, ||)) s of (new_s, a) -> unIO (       (\ch1 -> IO (\ s -> case (\world -> (world, ||)) s of (new_s, a) -> unIO (( \ch2 -> IO \world -> (world, ()) ) a) new_s) )             a) new_s) 123
     case (\world -> (world, ||)) 123 of (new_s, a) -> unIO (       (\ch1 -> IO (\ s -> case (\world -> (world, ||)) s of (new_s, a) -> unIO (( \ch2 -> IO \world -> (world, ()) ) a) new_s) )             a) new_s
     -- DING DING! 123  became 456, as is the way of the magic (State# RealWorld) value
     -- and || sucked up a "y" from the outside world
     case (456, "y") of (new_s, a) -> unIO (       (\ch1 -> IO (\ s -> case (\world -> (world, ||)) s of (new_s, a) -> unIO (( \ch2 -> IO \world -> (world, ()) ) a) new_s) )             a) new_s
     unIO (       (\ch1 -> IO (\ s -> case (\world -> (world, ||)) s of (new_s, a) -> unIO (( \ch2 -> IO \world -> (world, ()) ) a) new_s) )      "y") 456
     unIO (       IO (\ s -> case (\world -> (world, ||)) s of (new_s, a) -> unIO (( \ch2 -> IO \world -> (world, ()) ) a) new_s)  ) 456
     -- note "y" vanishes b/c we don't use it
     (\ s -> case (\world -> (world, ||)) s of (new_s, a) -> unIO (( \ch2 -> IO \world -> (world, ()) ) a) new_s)   456
     case (\world -> (world, ||)) 456 of (new_s, a) -> unIO (( \ch2 -> IO \world -> (world, ()) ) a) new_s
     -- DING DING! 456  became 789 and || sucked up a "z" from the outside world
     case (789, "z") of (new_s, a) -> unIO (( \ch2 -> IO \world -> (world, ()) ) a) new_s
     unIO (( \ch2 -> IO \world -> (world, ()) ) "z") 789
     -- note "z" vanishes b/c we don't use it
     unIO ( IO \world -> (world, ()) ) 789
     \world -> (world, ()) 789
     (789, ())
     -- main returns this lonesome useless tuple, now that the program has
     -- finished running. But wait, this doesn't match up with IO (), which
     -- is the type of main. Is it because I discarded IO once main kicked off
     -- with a world int? Even then, the return would be IO (780, ()) which
     -- seems off. Oh well?

     -- note that I'm basically guessing at the usage of || and 123 etc,
     -- and also guessing at the order of evaluation. Look in to the reality of
     -- these things.  

     -- note that 123 isn't really 123 or 456 etc. Read more below:
{-
the State# RealWorld is simply used to preserve statement order:
"GHC uses impure primitives to implement these IO operations (getChar etc); the State# RealWorld
"values" are only to stop the compiler reordering statements by introducing
DATA DEPENDENCIES from one statement to the next."*
... it's not actually the entire state of the world! It's just some arbitrary number. And I'm
not sure if the number changes or not from one impure IO to the next.

* but, I've read also that case statements are strict in Haskell, and some have
implied that this is the reason IO is guaranteed to be ordered. I could have
misunderstood. Here: "The repeated case-expressions, however, ensure our optimizer
                     doesn’t reorder the IO instructions (since that would have
                     a very observable effect!)"
                    (http://blog.ezyang.com/2011/05/unraveling-the-mystery-of-the-io-monad/)
... but maybe he means they won't get reordered b/c the pattern-matched value is actually 
used in subsequent statements, which is a DATA DEPENDENCY. But why would case
be necessary, couldn't we just do lets, or wheres, as in https://wiki.haskell.org/IO_inside ?


So in the IO's >>=, is it the usage of case, the usage of State RealWorld, or
both that ensures order?


Note also:
The IO monad is not a state monad, despite popular belief. The state monad looks very similar to the type GHC uses for IO. Thus the confusion.
Notice the reversed order of values in state vs io return tuple.
(from http://stackoverflow.com/questions/9244538/what-are-the-definitions-for-and-return-for-the-io-monad)
See for yourself:
https://hackage.haskell.org/package/base-4.9.0.0/docs/src/GHC.IO.html
https://hackage.haskell.org/package/base-4.9.0.0/docs/src/GHC.ST.html
-}













