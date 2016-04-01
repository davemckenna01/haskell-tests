{-
0
50
10

30
5
90

20
40
2

25
10
8
-}

roads :: [Int]
roads = [50, 10, 0,   5, 90, 30,   40, 2, 20,   10, 8, 25]


findPath :: [Int] -> Int -> [Int]
findPath path  = foldl decide (head path) path
    where road (x:y:ys) road
        | 

