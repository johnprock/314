import System.Environment
import Data.List


isKeyWord :: String -> Bool
isKeyWord s | s == "@title"      = True
			| s == "@section"    = True
			| s == "@subsection" = True
			| s == "@block"      = True
			| otherwise          = False
			
 
main = do
	[f,g] <- getArgs --code taken from haskell.org
	s     <- readFile f
	writeFile g s
	
	