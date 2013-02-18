import System.Environment
import Data.List


isKeyWord :: String -> Bool
isKeyWord s | head s ==  '@' = True
			| otherwise = False
			
 
main = do
	[f,g] <- getArgs --code taken from haskell.org
	s     <- readFile f
	writeFile g s
	
	--test