import System.Environment
import Data.List

data KeyWords = Title | Section | Subsection | Block | Plaintext


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

convertDoc :: [(Keywords, String)] -> String
converDoc x:xs 
	| fst x == Title      = makeTitle (snd x)      : (convertDoc xs)
	| fst x == Section    = makeSection (snd x)    : (convertDoc xs)
	| fst x == Subsection = makeSubsection (snd x) : (convertDoc xs)
	| fst x == Block      = makeBlock (snd x)      : (convertDoc xs)
	| fst x == Plaintext  = makePlaintext(snd x)   : (convertDoc xs)



isKeyWord :: String -> Bool
isKeyWord s | head s ==  '@' = True
			| otherwise = False
			
			
 
main = do
	[f,g] <- getArgs --code taken from haskell.org
	s     <- readFile f
	writeFile g s
	
	--test