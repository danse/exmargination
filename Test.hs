import Test.Hspec
import Tags (getTags)
import TagClustering (autoCategoriseAll)
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

preservesLength :: [Margin] -> Bool
preservesLength m = length m == length p
  where p = autoCategoriseAll m

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
    it "preserves length" $ property preservesLength
