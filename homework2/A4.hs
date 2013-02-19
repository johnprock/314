-- Kalil Armstrong
-- Patrick Rock
-- CSCE 314 Assignment 4

-- Note: At present, block uses parantheses instead of braces. Without doing a parity check, it's ambiguous as to how to handle braces inside of a code snippet.

import System.Environment
import Data.List
import Data.List.Split


-- Template generator
-- Take the output, and then cons it with the stuff I want at the beginning!
makeTemplate :: String -> String
makeTemplate s = "<head>" ++ "\n" ++ "<style>" ++ "body" ++ "\n" ++ "{" ++ "background-color:#000000;" ++ "\n" ++ "text-align:center;" ++ "\n" ++ "height:100%;" ++ "\n" ++ "margin:auto;" ++ "\n" ++ "width: 960px;" ++ "\n" ++ "color: FFFFFF;" ++ "\n" ++ "text-align:center;" ++ "\n" ++ "}" ++ "\n" ++ "h1" ++ "\n" ++ "{" ++ "\n" ++ "text-align:mid;" ++ "\n" ++ "\n" ++ "background-color:C0C0C0;" ++ "\n" ++ "color:3333FF;" ++ "\n" ++ "font-size:24pt;" ++ "\n" ++ "}" ++ "\n" ++"h3" ++ "\n" ++ "{" ++ "\n" ++ "background-color:#C0C0C0;" ++ "\n" ++ "color:3333FF;" ++ "\n" ++ "text-align:mid;" ++ "\n" ++ "font-size:18pt;" ++ "\n" ++ "}" ++ "\n" ++ "</style>" ++ "\n" ++ "</head>" ++ generate s

-- CONVERTER FUNCTIONS --
-- This will set items into the correct HTML format.
makeTitle :: String -> String 
makeTitle s = "<title>" ++ s ++ "</title>"

makeSection :: String -> String
makeSection s = "<h1>" ++ s ++ "</h1>"

makeSubsection :: String -> String
makeSubsection s = "<h3>" ++ s ++ "</h3>"

makePlaintext :: String -> String
makePlaintext s = "<p>" ++ s ++ "</p>"

makeBlock :: String -> String
makeBlock s = "<body>" ++ s ++ "</body>"

-----------------------------
-- LEXICAL ANALYSIS --
-- Finds tags.
getTag :: String -> String
getTag ('@':'t':'i':'t':'l':'e':xs) = "title"
getTag ('@':'s':'e':'c':'t':'i':'o':'n':xs) = "section"
getTag ('@':'s':'u':'b':'s':'e':'c':'t':'i':'o':'n':xs) = "subsection"
getTag ('@':'b':'l':'o':'c':'k':xs) = "block"
getTag s = "plaintext"

-------------------------
-- Pulls the "content" out of the keyword.
extractArgs :: String -> String
extractArgs s 
	| getTag s == "plaintext" = takeWhile (/= '@') s
	| otherwise = splitOneOf "()" s !! 1

-- Removes the keyword.
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

-- Converts the tuple of keyword, content into a string.
convertDoc :: [(String, String)] -> String
convertDoc [] = []
convertDoc (("title", content):xs) = makeTitle content ++ convertDoc xs
convertDoc (("section", content):xs) = makeSection content ++ convertDoc xs
convertDoc (("subsection", content):xs) = makeSubsection content ++ convertDoc xs
convertDoc (("plaintext", content):xs) = makePlaintext content ++ convertDoc xs
convertDoc (("block", content):xs) = makeBlock content ++ convertDoc xs

-- Magic.
generate :: String -> String
generate s = convertDoc (parse s)

 
main = do
	[f,g] <- getArgs --code taken from haskell.org
	s     <- readFile f
	writeFile g (makeTemplate s)
	