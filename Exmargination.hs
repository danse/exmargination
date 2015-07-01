{-# LANGUAGE DeriveDataTypeable,DeriveGeneric #-}
module Exmargination where

import ToTimeSeries as To
import Data.Time.Clock( NominalDiffTime, UTCTime )
import Data.Aeson( FromJSON, ToJSON )
import Data.Typeable
import Data.Data
import GHC.Generics

data Margin = Margin {
  value :: Float,
  description :: String,
  time :: UTCTime
  } deriving (Show, Data, Typeable, Generic)

instance FromJSON Margin
instance ToJSON Margin

merge :: UTCTime -> Margin -> Margin -> Margin
merge t m1 m2 = Margin {
  value = (value m1) + (value m2),
  description = (description m1) ++ ", " ++ (description m2),
  time = t
  }

create t = Margin {
  value = 0,
  description = "no data related to this period",
  time = t
  }

interval = 60*60*24 :: NominalDiffTime -- one day

toDailySeries = To.convert create merge time interval
