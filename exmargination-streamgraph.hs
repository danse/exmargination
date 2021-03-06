import Margin
import System.Environment (getArgs)
import Streamgraph (streamgraph)
import Data.Text (pack)
import Tags (getTags)
import TagClustering (autoCategoriseAll)
import Control.Applicative( some )
import Options.Applicative
import Data.Monoid( (<>) )
import Data.Time.Clock (getCurrentTime)

toStreamData (Margin value desc time) = (pack desc, value, time)

data Options = Options {
  days :: Int,
  fill :: Bool,
  arguments :: [String]
  }

optionParser :: Parser Options
optionParser = Options
               <$> option auto (long "days" <> short 'd' <> Options.Applicative.value 1)
               <*> switch (long "fill" <> short 'f')
               <*> some (argument str (metavar "INPUT_MARGIN_FILES ..."))

optionParserInfo :: ParserInfo Options
optionParserInfo = info optionParser fullDesc

maybeFill False margins = return margins
maybeFill True margins = do
  t <- getCurrentTime
  return (Margin 0 "" t:margins)

main = do
  options <- execParser optionParserInfo
  margins <- getAllMargins (arguments options)
  maybeFilled <- maybeFill (fill options) margins
  (streamgraph (days options) . map toStreamData . autoCategoriseAll) maybeFilled
