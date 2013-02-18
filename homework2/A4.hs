import System.Environment
import Data.List


-- CONVERTER FUNCTIONS --
makeTitle :: String -> String 
makeTitle s = "<title>" ++ s ++ "</title>"

makeSection :: String -> String
makeSection s = "<section>" ++ s ++ "</section>"

makeSubsection :: String -> String
makeSubsection s = "<section>" ++ s ++ "</section>"

makePlaintext :: String -> String
makePlaintext s = s

makeBlock :: String -> String
makeBlock s = s
-----------------------------


convertDoc :: [(String, String)] -> String
convertDoc (("title", stuff):xs) = makeTitle stuff ++ convertDoc xs
convertDoc (("section", stuff):xs) = makeSection stuff ++ convertDoc xs
convertDoc (("subsection", stuff):xs) = makeSubsection stuff ++ convertDoc xs
convertDoc (("plaintext", stuff):xs) = makePlainText stuff ++ convertDoc xs
convertDoc (("block", stuff):xs) = makeBlock stuff ++ convertDoc xs



isKeyWord :: String -> Bool
isKeyWord s | head s ==  '@' = True
			| otherwise = False
			
	
 
main = do
	[f,g] <- getArgs --code taken from haskell.org
	s     <- readFile f
	writeFile g s
	