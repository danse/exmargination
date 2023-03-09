{-# LANGUAGE TupleSections #-}
import Data.Map (insertWith, toList, empty)
import Margin (onAllMargins, value, description)
import System.Environment (getArgs)
import Data.List (sortOn)
import Data.Tuple (snd)

valToTags margin = fmap (, value margin) $ words $ description margin

expand margins = foldMap valToTags margins

reduce tagsAndVals = foldr insert empty tagsAndVals
  where insert (t, v) = insertWith (+) t v

process = unlines . map show . reverse . sortOn snd . toList . reduce . expand

main = do
  args <- getArgs
  onAllMargins args process
