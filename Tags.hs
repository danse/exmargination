module Tags where

import Data.String (words)
import qualified Data.Set as Set

isTagStart c = c == '#'

isTag [] = False
isTag (x:xs) =  isTagStart x && (length xs > 0)

truncateTag = fst . (break isTagStart)

clean = truncateTag . (drop 1)

getTags = (Set.toList . Set.fromList) . (map clean) . (filter isTag) . words

