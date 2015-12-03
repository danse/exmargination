import Collect( convertEncode )
import System.Environment
import Data.ByteString.Lazy.Char8( pack, unpack )
import Data.Functor( fmap )
import Control.Applicative( some )
import Options.Applicative

data Options = Options {
  days :: Int,
  inputs :: [String],
  output :: String
  }

optionParser :: Parser Options
optionParser = Options
               <$> option auto (long "days" <> short 'd' <> value 1)
               <*> some (strOption (long "input"
                                    <> short 'i'
                                    <> metavar "INPUT_MARGIN_FILE"))
               <*> strOption (long "output"
                              <> short 'o'
                              <> metavar "OUTPUT_COLLECTION_FILE")

optionParserInfo :: ParserInfo Options
optionParserInfo = info optionParser fullDesc

collection :: Options -> IO ()
collection Options {days = d, inputs = i, output = o} = 
  let convert = (convertEncode d) . (map pack)
      write = (writeFile o) . unpack
  in do
    strings <- sequence $ map readFile i
    write $ convert strings

main = execParser optionParserInfo >>= collection
