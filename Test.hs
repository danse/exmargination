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

main :: IO ()
main = hspec $ do
  describe "convert" $ do
    it "first fixture" $ runCase "1"
    it "simple fixture" $ runCase "2"
    it "input with a first element in wrong date order" $ runCase "3"
  describe "tags" $ do
    it "parses tags" ((getTags "#one two #three") `shouldBe` ["one", "three"])
    it "eliminates doubles" ((getTags "#one #one") `shouldBe` ["one"])
    it "understands truncations" ((getTags "#trun#cated") `shouldBe` ["trun"])
    it "ignores multiple truncations" ((getTags "#trun#cat#ed") `shouldBe` ["trun"])
