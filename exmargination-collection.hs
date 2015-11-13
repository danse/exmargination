import Collect( convertEncode )
import System.Environment
import Data.ByteString.Lazy.Char8( pack, unpack )
import Data.Functor( fmap )

main = do
  args <- getArgs
  if (length args) < 2
    then
    do
      prog <- getProgName
      putStrLn $ "usage: " ++ prog ++ " <margin> [<margin>...] <result>"
      putStrLn $ "this will read the `margin` file or files, convert and write to `result`"
    else
    let inputs = init args
        output = last args
        convert = convertEncode . (map pack)
        write = (writeFile output) . unpack
        complain e = print $ "parsing error: "++ e
    in do
      i <- sequence $ map readFile inputs
      write $ convert i
