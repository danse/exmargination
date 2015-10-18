import Collect( convertEncode )
import System.Environment
import Data.ByteString.Lazy.Char8( pack, unpack )
import Data.Functor( fmap )

main = do
  args <- getArgs
  if (length args) /= 2
    then
    do
      prog <- getProgName
      putStrLn $ "usage: " ++ prog ++ " <margin> <result>"
      putStrLn $ "this will read the `margin` file, convert and write to `result`"
    else
    let [input, output] = args
        convert i = convertEncode (pack i)
        write o = writeFile output (unpack o)
        complain e = print $ "parsing error: "++ e
    in do
      i <- readFile input
      either complain write $ convert i
