import Margin
import System.Environment (getArgs)
import Streamgraph (streamgraph)
import Data.Text (pack)
import Tags (getTags)

toStreamData (Margin value desc time) = (pack tagOrNot, value, time)
  where tags = getTags desc
        tagOrNot = if (length tags > 0) then (head tags) else "untagged"

main = do
  args <- getArgs
  margins <- getAllMargins args
  (streamgraph . map toStreamData) margins
