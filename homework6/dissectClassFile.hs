import qualified Data.ByteString as B


main :: IO()
main = do
  filename <- getLine
  contents <- B.readFile filename
  B.putStr contents
