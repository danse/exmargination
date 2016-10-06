module Tags where

import Data.String (words)
import qualified Data.Set as Set

isTag [] = False
isTag (x:xs) = x == '#' && (length xs > 0)

getTags = (Set.toList . Set.fromList) . (map (drop 1)) . (filter isTag) . words

