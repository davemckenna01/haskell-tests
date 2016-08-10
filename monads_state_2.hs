import Control.Monad.State

--state :: (s -> (a, s)) -> State s a

countList :: [Int] -> State Int ()
countList list = state $ \s -> ((), s + length list) 


count :: State Int ()
--count = do
--    countList [1,2,3]
--    countList [4,5,6]

-- OR 

count = countList [1,2,3] >>= \_ -> countList [4,5,6]

--runState count 0
-- becomes ((),6)


-- sloppy attempt to reduce the expression

count = countList [1,2,3] >>= \_ -> countList [4,5,6]
= \s -> ((), s + length [1,2,3]) >>= \_ -> \s -> ((), s + length [4,5,6])
...


