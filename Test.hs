import Test.Hspec
import Exmargination( Margin, toDailySeries )
import Collect( convert, decodeAnalyses, Analysis )
import Data.ByteString.Lazy.Char8( pack )
import Data.Aeson( eitherDecode )

main :: IO ()
main = hspec $ do
  describe "convert" $ do
    it "converts the simple fixture as expected" $ do
      input <- readFile "fixtures/2/input.json"
      output <- readFile "fixtures/2/output.json"
      (convert (pack input)) `shouldBe` (decodeAnalyses (pack output))
    it "converts the first fixture as expected" $ do
      input <- readFile "fixtures/1/input.json"
      output <- readFile "fixtures/1/output.json"
      (convert (pack input)) `shouldBe` (decodeAnalyses (pack output))
    it "converts input with a first element in wrong date order" $ do
      input <- readFile "fixtures/3/input.json"
      output <- readFile "fixtures/3/output.json"
      (convert (pack input)) `shouldBe` (decodeAnalyses (pack output))
