module Exmargination where

import ToTimeSeries as To
import Data.Time.Clock( NominalDiffTime )
import Data.DateTime( DateTime, startOfTime )

import Margin

instance Monoid Margin where
  mempty = Margin {
    value = 0,
    description = "no data related to this period",
    time = startOfTime
    }
  mappend m1 m2 =
    let (Margin { value = v1, description = d1, time = t1 }) = m1
        (Margin { value = v2, description = d2, time = t2 }) = m2
    in Margin {
      value = v1 + v2,
      description = d1 ++ ", " ++ d2,
      time = t1
      }

instance To.Timeserializable Margin where
  getTime = time
  setTime t m = m {time = t}

oneDay = 60*60*24 :: NominalDiffTime

toDailySeries :: Int -> [Margin] -> [Margin]
toDailySeries days = To.convert (oneDay * (fromIntegral days))

toDailySeriesFill :: DateTime -> Int -> [Margin] -> [Margin]
toDailySeriesFill dateTime days = To.convertFill dateTime (oneDay * (fromIntegral days))
