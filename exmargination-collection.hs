import Collect( convertEncode )
import System.Environment
import Data.ByteString.Lazy.Char8( pack, unpack )

main = do
  args <- getArgs
  if (length args) /= 2
    then
    do
      prog <- getProgName
      putStrLn $ "usage: " ++ prog ++ "<margin> <result>"
      putStrLn $ "this will read the `margin` file, convert and write to `result`"
    else
    let [input, output] = args
    in do
      i <- readFile input
      writeFile output (unpack (convertEncode (pack i)))
