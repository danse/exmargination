import Exmargination as Ex

data Average = Average {
  size :: Int,
  value :: Float
}

data Analysis = Analysis {
  date :: Date,
  averages :: [Average]
}

collectionToAverage :: [Ex.Margin] -> Averages
collectionToAverage c = Average { size=l, value=s/l}
  where l = len c,
        s = (foldl1 sum) $ map value c

collectionToAverages :: [Ex.Margin] -> [Averages]
collectionToAverages [m] = collectionToAverage [m]
collectionToAverages (mHead:mTail) = a1 ++ a2
  where a1 = collectionToAverage mHead,
        a2 = collectionToAverage mTail

analyse :: [Ex.Margin] -> Analysis
analyse collection = Analysis { date=date, averages=averages }
  where date = time (head collection),
        averages = collectionToAverages collection

data Folded = Folded {
  analyses :: [Analysis],
  collection :: [Ex.Margin]
}

collect :: [Ex.Margin] -> [Analysis]
collect margins
  | len margins > 1 = firstAnalysis : ((analyse . tail) margins)
  | len margins > 0 = firstAnalysis
  | otherwise       = []
  where firstAnalysis = analyse margins

convert = collect . Ex.toDailySeries
