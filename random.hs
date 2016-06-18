import System.Random

threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)  



threeCoins (mkStdGen 21)  
(True,True,True)  
threeCoins (mkStdGen 21)  
(True,True,True)  
threeCoins (mkStdGen 21)  
(True,True,True)  

-- ALWAYS the same result (purity)

threeCoins (mkStdGen 22)  
(True,False,True)  
threeCoins (mkStdGen 943)  
(True,False,True)  
threeCoins (mkStdGen 944)  
(True,True,True) 


mkStdGen 21 -- will always return:
22 1 -- note the weird return value, 2 numbers separated, and also that the left
     -- return val is always just incr by 1

random (mkStdGen 21) -- will always return:
(7214732918434336375,1703244511 2103410263)

-- So say you built a casino app where you win if you guess the computer's flip.
-- computer flip =
threeCoins (mkStdGen 21)  
(True,True,True) 
-- You could leak the source code and see that it's seeded with 21, go to your
-- computer and open ghci and type threeCoins (mkStdGen 21) and then know
-- exactly what to guess, because it's the same always.

-- THE WAY AROUND THIS is to use getStdGen, which basically chooses a new seed
-- to mkStdGen, and that new seed is DIFFERENT EACH TIME THE CODE IS COMPILED
-- (or run, I'm not sure):

getStdGen
418864242 1
-- close and re-open ghci:
getStdGen
1653653589 1

-- note also that:
mkStdGen 1653653588
1653653589 1

-- BUT NOTE THAT getStdGen returns IO StdGen, which is a monad and has to be
-- worked with monad-style. So you can't do:
random getStdGen
-- bc random expects a StdGen, not and IO StdGen

-- so you'd have to do:

showRandom :: IO StdGen -> IO (Int, StdGen)
showRandom gen = do
    g <- gen
    return $ random g
