import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid

money = Map.fromList [("dave", Sum 10), ("gavin", Sum 11)]

{-
    {
        "dave": 10,
        "gavin": 11
    }

-}

getMoney name = Map.lookup name money

davesMoney = getMoney "dave"
gavinsMoney = getMoney "gavin"
zorbulonsMoney = getMoney "zorbulon"

total = davesMoney `mappend` gavinsMoney `mappend` zorbulonsMoney
-- => Just (Sum {getSum = 21})

\x -> x + 1

