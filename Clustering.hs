module Clustering where

import qualified Data.Map as M
import Data.Maybe (isJust, catMaybes)
import Data.List (sortOn, filter, uncons)
import Data.Functor (fmap)

i = [
  [1, 2, 3],
  [1, 3],
  [1],
  [2, 4],
  [1, 2],
  []
  ]

s = M.fromList [
  (1, 4),
  (2, 3),
  (3, 2),
  (4, 1)
  ]

-- | get stats
-- >>> getStats i
-- fromList [(1,4),(2,3),(3,2),(4,1)]
getStats :: Ord a => [[a]] -> M.Map a Integer
getStats = foldr (\x -> M.insertWith (+) x 1) M.empty . concat

sndMaybe :: (a, Maybe b) -> Maybe (a, b)
sndMaybe (a, Just b) = Just (a, b)
sndMaybe (a, Nothing) = Nothing

-- | lookup and sort
-- >>> lookupAndSort (M.fromList [(1, 4), (2, 1)]) [1, 1, 2]
-- [(2,1),(1,4),(1,4)]
lookupAndSort :: (Ord k, Ord a) => M.Map k a -> ([k] -> [(k, a)])
lookupAndSort s = sortOn snd . catMaybes . map (\ k -> sndMaybe (k, M.lookup k s))

sortSnd f s = catMaybes . map ((fmap (fst . fst)) . uncons . f . lookupAndSort s)

-- | majority association
-- >>> majority s i
-- [1,1,1,2,1]
majority :: (Ord k, Ord a) => M.Map k a -> [[k]] -> [k]
majority = sortSnd reverse
-- | minority association
-- >>> minority s i
-- [3,3,1,4,2]
minority :: (Ord k, Ord a) => M.Map k a -> [[k]] -> [k]
minority = sortSnd id


-- | cluster
-- >>> let i = [[1, 2, 3], [2, 3], [3]]
-- >>> cluster majority i
-- [3,3,3]
-- >>> cluster minority i
-- [1,2,3]
cluster f i = f (getStats i) i