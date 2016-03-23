module Exmargination where

import ToTimeSeries as To
import Data.Time.Clock( NominalDiffTime )

import Margin

instance To.Timeserializable Margin where
  access = time

  merge t m1 m2 =
    let (Margin { value = v1, description = d1 }) = m1
        (Margin { value = v2, description = d2 }) = m2
    in Margin {
      value = v1 + v2,
      description = d1 ++ ", " ++ d2,
      time = t
      }

  fill t = Margin {
    value = 0,
    description = "no data related to this period",
    time = t
    }

oneDay = 60*60*24 :: NominalDiffTime

toDailySeries :: Int -> [Margin] -> [Margin]
toDailySeries days = To.convert (oneDay * (fromIntegral days))
