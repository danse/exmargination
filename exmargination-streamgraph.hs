import Margin
import System.Environment (getArgs)
import Streamgraph (streamgraph)
import Data.Text (pack)
import Tags (getTags)
import TagClustering (autoCategorise)

toStreamData (Margin value desc time) = (pack desc, value, time)

{-#
toStreamData (Margin value desc time) = (pack tagOrNot, value, time)
  where tags = getTags desc
        tagOrNot = if (length tags > 0) then (head tags) else "untagged"
#-}

mapToDescs f margins = map updateDesc (zip descs margins)
  where descs = (f . map description) margins
        updateDesc (desc, margin)  = margin { description = desc }

main = do
  args <- getArgs
  margins <- getAllMargins args
  (streamgraph . map toStreamData . mapToDescs autoCategorise) margins
  --(streamgraph . map toStreamData) margins
