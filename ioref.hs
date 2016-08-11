import Data.IORef


{-
IORefs (short for REFerence?) represent a mutable cell, and you want to make sure you're reading and
writing that cell in a certain order, otherwise your algorithm might make no
sense.

Genius line from http://research.microsoft.com/en-us/um/people/simonpj/papers/marktoberdorf/mark.pdf:
"In this example, the mutable variable models (part of) the state of the file being written to, by tracking the
number of characters written to the file. Since THE FILE ITSELF IS, IN EFFECT, AN EXTERNAL MUTABLE VARIABLE, it is
not surprising that an internal mutable variable is appropriate to model its state."

There's really no difference in the monadic need for working with IORefs (mutable variables)
vs external files. IT'S THE SAME PATTERN!
-}

main = do
     x <- newIORef 42
     writeIORef x 43
     modifyIORef x (+1)
     val <- readIORef x
     print val -- will be 44