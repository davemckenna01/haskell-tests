{-
meant to be loaded into ghci, like:
$ ghci every_other.hs
> everyOther [1,2,3,4]
[2,4]
-}

everyOther1 list acc keep =
  if null list
    then reverse acc
  else if null acc && keep == False
    then everyOther1 (tail list) acc True
  else if keep == True
    then everyOther1 (tail list) ((head list):acc) False
  else everyOther1 (tail list) acc True


everyOther2 [] (y:ys) _     = reverse (y:ys)
everyOther2 (x:xs) [] False = everyOther2 xs [] True
everyOther2 (x:xs) acc keep
                            | keep == True  = everyOther2 xs (x:acc) False
                            | keep == False = everyOther2 xs acc True


-- todo, rewrite with modulo to enable "every n elements"


everyOther list = everyOther2 list [] False


