{-

The best path from source to destination is the shorter of the paths to A4
or B4.

To get those paths, you consume the road network from left to right.
Along the way, you keep track of the shortest path to each intersection.
When you depart along a road, to take the shortest path in that section, you
depart from the intersction on the road you're departing on, and since we've
kept track of the shortest paths to all intersctions, we know which one to take
before we depart on the new road.

What's the shortest path to A1? It's B, C for 40.
What's the shortest path to B1? It's B for 10.

What's the shortest path to A2? It's A for 5.
And since we're travelling along A from A1, we prepend the shortest path to A1,
which is B, C for 40.
So, the shortest path to A2 is B, C, A for 45.

What's the shortest path to B2? It's A, C for 25.
And since we're leaving from A1, we prepend the shortest path to A1,
which is B, C for 40.
So, the shortest path to B2 is B, C, A, C for 65.

        A0 - - 50 - - A1 - - 5  - - A2 - - 40 - - A3 - - 10 - - A4
                      |             |             |             |
SOURCE              C 30          C 20          C 25          C 0   DESTINATION
                      |             |             |             |
        B0 - - 10 - - B1 - - 90 - - B2 - - 2  - - B3 - - 8  - - B4

-}

-- define simple data types to use
data Section = Section Int Int Int deriving (Show)
type RoadNetwork = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

-- an atomic operation we'll need is taking a section and returning the best
-- paths to the A and B intersections in that section.

-- we want to consume the road network form left to right, and accumulate
-- the best path as we go. This smells like a fold! So lets define a binary
-- operator:
processSection :: (Path, Path) -> Section -> (Path, Path)
processSection (pathA, pathB) (Section a b c) = 
    let pathACost = sum $ map snd pathA
        pathBCost = sum $ map snd pathB
        aXFromA = pathACost + a
        aXFromB = pathBCost + b + c
        bXFromB = pathBCost + b
        bXFromA = pathACost + a + c
        bestPathToA = if aXFromA <= aXFromB
                      then (A, a):pathA -- cheaper to add to head of list, and reverse later
                      else (C, c):(B, b):pathB
        bestPathToB = if bXFromB <= bXFromA
                      then (B, b):pathB
                      else (C, c):(A, a):pathA
    in (bestPathToA, bestPathToB)

bestPath :: RoadNetwork -> Path
bestPath road = 
    let (bestA, bestB) = foldl processSection ([], []) road
    in if sum (map snd bestA) <= sum (map snd bestB)
        then reverse bestA
        else reverse bestB

input :: [Int]
input = [50, 10, 30, 5, 90, 20, 40, 2, 25, 10, 8, 0]

groupsOf :: Int -> [a] -> [[a]]  
groupsOf 0 _ = undefined  
groupsOf _ [] = []  
groupsOf n xs = take n xs : groupsOf n (drop n xs)  

road = map (\[a,b,c] -> Section a b c ) $ groupsOf 3 input

{-
> bestPath road
=> [(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)]
-}
