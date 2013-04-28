-- Takes a filename and encryption option as CLI
-- encrypts the file and writes it back out
-- Kalil Armstrong: RC4
-- Patrick Rock:    ROT13

import System.Environment
import Data.String

-- Patrck's Function
rot13 :: String -> String
rot13 s = "rot13"

-- Kalil's Function
rc4 :: String -> String
rc4 s = "rc4"

encrypt :: String -> (String -> String)
encrypt opt
  | opt == "rc4"    = rc4
  | opt == "rot13"  = rot13

main :: IO()
main = do
  [inp,out,opt] <- getArgs
  s         <- readFile inp
  writeFile out (encrypt opt s)

