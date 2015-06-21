import ToTimeSeries as To

data Margin = Margin {
  value :: Float,
  description :: String,
  time :: String
  } deriving Show

merge :: Margin -> Margin -> Margin
merge m1 m2 = Margin {
    value = (value m1) + (value m2),
    description = (description m1) ++ ", " ++ (description m2),
    time = time m1
  }

readMarginFile

convertToDailySeries
