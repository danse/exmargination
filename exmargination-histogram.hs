{-# LANGUAGE OverloadedStrings #-}
import GHC.Float (float2Double)
import Margin (getAllMargins, value)
import System.Environment (getArgs)

-- import qualified Dataframe as D
import qualified Data.Text.IO as T
import qualified Data.Text as T

import Granite.Svg

{-
marginToDataframe :: [Margin] -> _
marginToDataframe m =
  D.fromNamedColumns
  [ ( "value", D.fromList $ value <$> m)
  , ( "description", D.fromList $ description <$> m)
  , ( "time", D.fromList $ time <$> m)
  ]
-}

main = do
  args <- getArgs
  m <- getAllMargins args
  let values = float2Double . value <$> m
  T.putStrLn $
    histogram
    (bins 30 (minimum values) (maximum values))
    values
    defPlot
    { legendPos = LegendBottom
    , xFormatter = \_ _ v -> T.pack (show (round v :: Int))
    , xNumTicks = 10
    , yNumTicks = 5
    , plotTitle = "Margin Values (hours)"
    }
