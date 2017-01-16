import Test.Hspec
import Tags( getTags )
import Data.ByteString.Lazy.Char8( pack )
import Data.Aeson( eitherDecode )
import Data.Functor( fmap )

tagCase input output = it input ((getTags input) `shouldBe` output)

main :: IO ()
main = hspec $ do
  describe "getTags" $ do
    tagCase "#one two #three" ["one", "three"]
    tagCase "#one #one" ["one"]
    tagCase "#trun#cated" ["trun"]
    tagCase "#trun#cat#ed" ["trun"]
