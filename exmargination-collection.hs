{-# LANGUAGE RecordWildCards #-}
import Collect( convertEncode, convertEncodeWithTime )
import System.Environment
import Data.ByteString.Lazy.Char8( pack, unpack )
import Data.Functor( fmap )
import Control.Applicative( some )
import Options.Applicative
import Data.DateTime( getCurrentTime )

data Options = Options {
  days :: Int,
  fill :: Bool,
  inputs :: [String],
  output :: String
  }

optionParser :: Parser Options
optionParser = Options
               <$> option auto (long "days" <> short 'd' <> value 1)
               <*> switch (long "fill" <> short 'f')
               <*> some (strOption (long "input"
                                    <> short 'i'
                                    <> metavar "INPUT_MARGIN_FILE"))
               <*> strOption (long "output"
                              <> short 'o'
                              <> metavar "OUTPUT_COLLECTION_FILE")

optionParserInfo :: ParserInfo Options
optionParserInfo = info optionParser fullDesc

collection :: Options -> IO ()
collection Options {..} = 
  let convert = (convertEncode days) . (map pack)
      convertWithTime t = (convertEncodeWithTime t days) . (map pack)
      write = (writeFile output) . unpack
  in do
    strings <- sequence $ map readFile inputs
    if fill
      then
      do
        dateTime <- getCurrentTime
        write $ convertWithTime dateTime strings
      else
      do
        write $ convert strings

main = execParser optionParserInfo >>= collection
