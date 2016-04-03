module ToTimeSeries where

import Data.DateTime( DateTime )
import Data.List( sortBy )
import Data.Time.Clock( NominalDiffTime,addUTCTime )
import Data.Ord( compare )

class Timeserializable a where
  access :: a -> DateTime
  merge :: DateTime -> a -> a -> a
  fill :: DateTime -> a

-- at every call, the recursive function returns a processed list, and
-- it gets a non processed list and a reference date about the last
-- emitted element. if the next element would have a date greater than
-- the reference + the interval, a filling element is
-- created. otherwise, the function will look ahead and merge all
-- elements within the same interval, pick a representative date for
-- the merged elements and use it as the new reference
consume :: Timeserializable a => [DateTime] -> [a] -> [a]
consume (t:ts) [] = []
consume (t:ts) elements
  | length preceding == 0 = (fill t) : rest
  | otherwise = (foldl (merge t) (fill t) preceding) : rest
  where (preceding, succeeding) = span ((<= t) . access) elements
        rest = consume ts succeeding

iterator :: NominalDiffTime -> DateTime -> [DateTime]
iterator interval start = iterate (addUTCTime interval) start

sort :: Timeserializable a => [a] -> [a]
sort = sortBy (\ x y -> compare (access x) (access y))

convert :: Timeserializable a => NominalDiffTime -> [a] -> [a]
convert interval elements =
  sampler sorted
  where sorted = sort elements
        times = iterator interval (access (head sorted))
        sampler = consume times

convertFill :: Timeserializable a => DateTime -> NominalDiffTime -> [a] -> [a]
convertFill dateTime interval elements = convert interval filledElements
  where filledElements = (fill dateTime):elements
