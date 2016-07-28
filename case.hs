f1 x = case x of 1 -> "a one!"

-- can't do this: Error: Not in scope: data constructor ‘Int’
--f2 x = case x of Int -> "an Int!"

f2 x = case x of (a,b) -> "a tuple!"

--bindIO :: IO a -> (a -> IO b) -> IO b
--bindIO (IO m) k = IO (\ s -> case m s of (# new_s, a #) -> unIO (k a) new_s)

--unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
--unIO (IO a) = a


a1 = Just (\ _ -> ("foo", "bar") )
a2 x = Just (\ _ -> ("baz", "bop") )


--bindMab :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMab (Just m) f = Just (\ s -> case m s of (new_s, a) -> unMab (f a) new_s)

--unMab :: Maybe a -> (Int -> (Int, a))
unMab (Just a) = a




