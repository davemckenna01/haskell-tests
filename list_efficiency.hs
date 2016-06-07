-- be careful when appending things to a list. There's an ineffecient way to
-- do it.

-- the way ++ is defined, it requres the left arg to be a list, so if it sees
-- (a ++ b) as the left arg, it goes ahead and constructs (a ++ b) into a list
-- before appending the new list items. This is inefficient. Better to make sure
-- the left arg is already a list.

(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys


a ++ (b ++ c) -- 4 steps

["a"] ++ (["b"] ++ ["c"])
"a" : [] ++ (["b"] ++ ["c"])
"a" : (["b"] ++ ["c"])
"a" : ("b" : ["c"])


(a ++ b) ++ c -- 6 steps (and think exponentially as list grows)

(["a"] ++ ["b"]) ++ ["c"]
("a" : [] ++ ["b"]) ++ ["c"]
("a" : ["b"]) ++ ["c"]
"a" : ["b"] ++ ["c"]
"a" : "b" : [] ++ ["c"]
"a" : "b" : ["c"]

-- One way to guard against accidentally implementing an inefficient list
-- is to use "Diff lists": http://learnyouahaskell.com/for-a-few-monads-more#writer (scroll down a bit)