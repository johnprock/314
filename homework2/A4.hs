import System.Environment
import Data.List
import Data.List.Split


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
convertDoc [] = []
convertDoc (("title", stuff):xs) = makeTitle stuff ++ convertDoc xs
convertDoc (("section", stuff):xs) = makeSection stuff ++ convertDoc xs
convertDoc (("subsection", stuff):xs) = makeSubsection stuff ++ convertDoc xs
convertDoc (("plaintext", stuff):xs) = makePlaintext stuff ++ convertDoc xs
convertDoc (("block", stuff):xs) = makeBlock stuff ++ convertDoc xs

-- LEXICAL ANALYSIS --

getTag :: String -> String
getTag ('@':'t':'i':'t':'l':'e':xs) = "title"
getTag ('@':'s':'e':'c':'t':'i':'o':'n':xs) = "section"
getTag ('@':'s':'u':'b':'s':'e':'c':'t':'i':'o':'n':xs) = "subsection"
getTag ('@':'b':'l':'o':'c':'k':xs) = "block"
getTag s = "plaintext"

-------------------------
			
extractArgs :: String -> String
extractArgs s 
	| getTag s == "plaintext" = takeWhile (/= '@') s
	| otherwise = splitOneOf "()" s !! 1


removeTag :: String -> String 
removeTag s 
			| getTag s == "plaintext" = (dropWhile (/= '@') s)
			| otherwise = tail (dropWhile (/= ')') s)

			
parse :: String -> [(String, String)]
parse s 
	| s == [] = []
	| getTag s == "title" = ("title",extractArgs s): (parse (removeTag s))
	| getTag s == "section" = ("section",extractArgs s): (parse (removeTag s))
	| getTag s == "subsection" = ("subsection",extractArgs s): (parse (removeTag s))
	| getTag s == "plaintext" = ("plaintext",extractArgs s): (parse (removeTag s))
	| getTag s == "block" = ("block",extractArgs s): (parse (removeTag s))	


generate :: String -> String
generate s = convertDoc (parse s)

 
main = do
	[f,g] <- getArgs --code taken from haskell.org
	s     <- readFile f
	writeFile g (generate s)
	