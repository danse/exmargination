import Data.Map (insertWith, toList, empty)
import Margin (onAllMargins, value, description)
import System.Environment (getArgs)
import Data.List (sortOn)
import Data.Tuple (snd)
import Tags (getTags)

valToTags margin = map addQuantity tags
  where tags = getTags (description margin)
        v = value margin
        addQuantity t = (t, v)

expand margins = (concat . (map valToTags)) margins

reduce tagsAndVals = foldr insert empty tagsAndVals
  where insert (t, v) = insertWith (+) t v

process = unlines . map show . reverse . (sortOn snd) . toList . reduce . expand

main = do
  args <- getArgs
  onAllMargins args process
