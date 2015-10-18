{-# LANGUAGE DeriveDataTypeable,DeriveGeneric #-}
module Collect where

import Exmargination( Margin, toDailySeries, value, time )
import Data.DateTime( DateTime )
import Data.Aeson( encode, eitherDecode, FromJSON, ToJSON )
import Data.Functor( fmap )
import Data.Typeable
import Data.Data
import GHC.Generics
import Data.ByteString.Lazy( ByteString )

data Average = Average {
  size :: Int,
  value :: Float
} deriving (Show, Typeable, Data, Generic, Eq)

instance FromJSON Average
instance ToJSON Average

data Analysis = Analysis {
  date :: DateTime,
  averages :: [Average]
} deriving (Show, Typeable, Data, Generic, Eq)

instance FromJSON Analysis
instance ToJSON Analysis

collectionToAverage :: [Margin] -> Average
collectionToAverage c = Average { size=l, Collect.value=v}
  where l = length c
        s = sum $ map Exmargination.value c
        v = s / (fromIntegral l)

collectionToAverages :: [Margin] -> [Average]
collectionToAverages [] = []
collectionToAverages coll = a:as
  where a = collectionToAverage coll
        as = collectionToAverages $ tail coll

analyse :: [Margin] -> Analysis
analyse collection = Analysis { date=date, averages=averages }
  where date = time (head collection)
        averages = reverse $ collectionToAverages $ reverse collection

collect :: [Margin] -> [Analysis]
collect [] = []
collect margins = ((collect . tail) margins) ++ [analyse margins]

convert :: [Margin] -> [Analysis]
convert = collect . reverse . toDailySeries

convertEncode toDecode = fmap (encode . convert) (eitherDecode toDecode)
