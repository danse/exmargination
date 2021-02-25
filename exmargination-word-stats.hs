import Data.Map (toList, fromListWith)
import Margin (onAllMargins, Margin(..))
import System.Environment (getArgs)
import Data.List (sortOn)
import Data.Tuple (snd)
import Tag.Clustering (autoCategoriseAll)

aggregateMargins :: [Margin] -> [(String, Float)]
aggregateMargins =
  let toPair (Margin f d _) = (d, f)
  in sortOn snd . toList . fromListWith (+) . fmap toPair

process :: [Margin] -> String
process = unlines . fmap show . aggregateMargins . autoCategoriseAll

wordStats :: [FilePath] -> IO ()
wordStats paths = onAllMargins paths process

main :: IO ()
main = getArgs >>= wordStats
