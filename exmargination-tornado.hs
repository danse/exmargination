{-# LANGUAGE RecordWildCards #-}
import Retornado (retornado)
import System.Environment
import Data.ByteString.Lazy.Char8( pack, unpack )
import Data.Functor( fmap )
import Data.Monoid( (<>) )
import Control.Applicative( some )
import Options.Applicative
import Margin
import qualified Data.Text as T

data Options = Options {
  days :: Int,
  fill :: Bool,
  arguments :: [String]
  }

optionParser :: Parser Options
optionParser = Options
               <$> option auto (long "days" <> short 'd' <> Options.Applicative.value 1)
               <*> switch (long "fill" <> short 'f')
               <*> some (argument str (metavar "INPUT_MARGIN_FILES ..."))

optionParserInfo :: ParserInfo Options
optionParserInfo = info optionParser fullDesc

toGraphData (Margin value desc time) = (T.pack desc, value, time)

tornado :: Options -> IO ()
tornado Options {..} = do
    margins <- getAllMargins arguments
    (retornado days . map toGraphData) margins

main = execParser optionParserInfo >>= tornado
