-- Takes a filename and encryption option as CLI
-- encrypts the file and writes it back out
-- Kalil Armstrong: RC4
-- Patrick Rock:    ROT13

import System.Environment


encrypt :: String -> String -> String
encrypt s opt = " "


main :: IO()
main = do
  [inp,out,opt] <- getArgs
  s         <- readFile inp
  writeFile out (encrypt s opt)

