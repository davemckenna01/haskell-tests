import Data.IORef

main = do
     x <- newIORef 42
     writeIORef x 43
     modifyIORef x (+1)
     val <- readIORef x
     print val -- will be 44