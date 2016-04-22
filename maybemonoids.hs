-- You can safely perform computations using computations that might not succeeded.

-- E.g., say there were three different processes that got each part of my name.
-- If one failed, our concatenation computation would still succeed,
-- it'd just have less information to work with.

let m1::Maybe String; m1 = Just "Dave"
let m2::Maybe String; m2 = Just "Scott"
let m3::Maybe String; m3 = Just "McKenna"

m1 `mappend` m2 `mappend` m3
Just "DaveScottMcKenna"

let m2::Maybe String; m2 = Nothing

m1 `mappend` m2 `mappend` m3
Just "DaveMcKenna"