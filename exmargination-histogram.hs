{-# LANGUAGE OverloadedStrings #-}
import GHC.Float (float2Double)
import Margin (getAllMargins, value)
import System.Environment (getArgs)

import qualified Data.Text.IO as T
import qualified Data.Text as T

import Granite.Svg

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
