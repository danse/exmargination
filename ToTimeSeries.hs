module ToTimeSeries where

import Data.DateTime( DateTime )
import Data.List( sortBy )
import Data.Time.Clock( NominalDiffTime,addUTCTime )
import Data.Ord( compare )

type Accessor a = a -> DateTime
-- the merger gets a list and a date to use for the resulting element
type Merger a = DateTime -> a -> a -> a
type Filler a = DateTime -> a

-- at every call, the recursive function returns a processed list, and
-- it gets a non processed list and a reference date about the last
-- emitted element. if the next element would have a date greater than
-- the reference + the interval, a filling element is
-- created. otherwise, the function will look ahead and merge all
-- elements within the same interval, pick a representative date for
-- the merged elements and use it as the new reference
consume :: Filler a -> Accessor a -> Merger a -> [DateTime] -> [a] -> [a]
consume create getDate aggregate (t:ts) [] = []
consume create getDate aggregate (t:ts) elements
  | length preceding == 0 = (create t) : rest
  | otherwise = (foldl (aggregate t) (create t) preceding) : rest
  where (preceding, succeeding) = span ((<= t) . getDate) elements
        rest = consume create getDate aggregate ts succeeding

iterator :: NominalDiffTime -> DateTime -> [DateTime]
iterator interval start = iterate (addUTCTime interval) start

sorter :: Accessor a -> [a] -> [a]
sorter acc = sortBy (\ x y -> compare (acc x) (acc y))

convert :: Filler a -> Merger a -> Accessor a -> NominalDiffTime -> [a] -> [a]
convert creator merger acc interval elements =
  sampler sorted
  where sorted = sorter acc elements
        times = iterator interval (acc (head sorted))
        sampler = consume creator acc merger times
