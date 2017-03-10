import Test.Hspec
import Tags (getTags)
import TagClustering (autoCategoriseAll, autoCategorise)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Aeson (eitherDecode)
import Data.Functor (fmap)
import Test.QuickCheck
import Margin
import Data.Time.Clock
import Data.Time.Calendar (fromGregorian)

tagCase input output = it input ((getTags input) `shouldBe` output)

-- make an universal time value using year, month, day
makeUni :: Integer -> Int -> Int -> UTCTime
makeUni y m d = UTCTime (fromGregorian y m d) (secondsToDiffTime 0)

preservesLength :: ([a] -> [a]) -> [a] -> Bool
preservesLength f m = length m == length (f m)

autoAllPreservesLength :: [Margin] -> Bool
autoAllPreservesLength = preservesLength autoCategoriseAll

autoCatPreservesLength = preservesLength autoCategorise

instance Arbitrary Margin where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    y <- arbitrary
    m <- arbitrary
    d <- arbitrary
    return (Margin a b (makeUni y m d))

main :: IO ()
main = hspec $ do
  describe "getTags" $ do
    tagCase "#one two #three" ["one", "three"]
    tagCase "#one #one" ["one"]
    tagCase "#trun#cated" ["trun"]
    tagCase "#trun#cat#ed" ["trun"]
  describe "autoCategoriseAll" $ do
    it "preserves length" $ property autoAllPreservesLength
  describe "autoCategorise" $ do
    it "preserves length" $ property autoCatPreservesLength
