import Data.Map (Map)
import qualified Data.Map as Map

people :: Map [Char] Int
people = Map.fromList [("dave", 33), ("kev", 24)]

isOld :: [Char] -> Maybe Bool
isOld name = Map.lookup name people >>= \age -> Just $ age >= 30



