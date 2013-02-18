import System.Environment
import Data.List

data Keyword = Title | Section | Subsection | Plaintext | Block

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

convertDoc :: [(Keyword, String)] -> String
convertDoc ((Title, stuff):xs) = makeTitle stuff ++ convertDoc xs
convertDoc ((Section, stuff):xs) = makeSection stuff ++ convertDoc xs
convertDoc ((Subsection, stuff):xs) = makeSubsection stuff ++ convertDoc xs
convertDoc ((Plaintext, stuff):xs) = makePlaintext stuff ++ convertDoc xs
convertDoc ((Block, stuff):xs) = makeBlock stuff ++ convertDoc xs

-- LEXICAL ANALYSIS --
isTitle :: String -> [(Keyword,String)]
isTitle ('@':'t':'i':'t':'l':'e':xs) = [(Title,"t")]

isSection :: String -> [(Keyword,String)]
isSection ('@':'s':'e':'c':'t':'i':'o':'n':xs) = [(Section,"t")]

isSubsection :: String -> [(Keyword,String)]
isSubsection ('@':'s':'u':'b':'s':'e':'c':'t':'i':'o':'n':xs) = [(Subsection,"t")]

isBlock :: String -> [(Keyword,String)]
isBlock ('@':'b':'l':'o':'c':'k':xs) = [(Block,"t")]
-------------------------





isKeyWord :: String -> Bool
isKeyWord s | head s ==  '@' = True
			| otherwise = False
			
	
 
main = do
	[f,g] <- getArgs --code taken from haskell.org
	s     <- readFile f
	writeFile g s
	