import Test.Hspec
import Collect( convert )
import Tags( getTags )
import Data.ByteString.Lazy.Char8( pack )
import Data.Aeson( eitherDecode )
import Data.Functor( fmap )

-- i cannot reuse the eitherDecode, because the resulting function
-- would have the same type signature, while i need different types
-- for analyses and margins
convertDecoded input = fmap (convert 1) (eitherDecode (pack input))
decoded = eitherDecode . pack 

runCase num = do
  input  <- readFile $ "fixtures/"++ num ++"/input.json"
  output <- readFile $ "fixtures/"++ num ++"/output.json"
  (convertDecoded input) `shouldBe` (decoded output)

tagCase input output = it input ((getTags input) `shouldBe` output)

main :: IO ()
main = hspec $ do
  describe "convert" $ do
    it "first fixture" $ runCase "1"
    it "simple fixture" $ runCase "2"
    it "input with a first element in wrong date order" $ runCase "3"
  describe "getTags" $ do
    tagCase "#one two #three" ["one", "three"]
    tagCase "#one #one" ["one"]
    tagCase "#trun#cated" ["trun"]
    tagCase "#trun#cat#ed" ["trun"]
