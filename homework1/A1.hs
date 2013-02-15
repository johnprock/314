-- Patrick Rock
--Prelude> (238900*1.609)/130
--2956.846923076923
----------------------------------------
-- e2 Patrick Rock
p 1 = 1
p x = (p (x-1)) + (p (x-1))

-----------------------------------------
-- Kalil Armstrong
-- Let them input a string
-- Check to see if it begins with a vowel; make a function to check it.

-- This defines what chkVowel is. This works. It takes a character, returns if it's not a vowel
-- I originally checked to see if it was a vowel; reaized it was much smarter to check if it's NOT
-- a vowel form a stack overflow post
chkVowel :: Char -> Bool
chkVowel v = notElem v ['a','e','i','o','u']

-- convert to pig latin
cpl :: String -> String
cpl [] = ""
cpl str@(x:xs) = if not (chkVowel x )
				then str ++ "yay"
				else let (a, b) = span chkVowel str
                    in (b ++ a ++ "ay")

-------------------------------------------------------
-- Kalil Armstrong	
cMap = [(1000,"M"), (900,"CM"), (500,"D"), (400,"CD"), (100,"C"), (90,"XC"), (50,"L"), 
		  (40,"XL"), (10,"X"), (9,"IX"), (5,"V"), (4,"IV"), (1,"I")]

-- Case for treating 0
-- I got this idea from StackOverflow
-- http://codereview.stackexchange.com/questions/5550/obvious-design-flaws-in-haskell-code-convert-to-roman-numerals
zeroRomanNumeral :: Integer -> String
zeroRomanNumeral 0 = "You've entered a zero; there is no representation for zero."
zeroRomanNumeral x = toRomanNumeral x

-- I got this idea initially from post https://piazza.com/class#spring2013/csce314/29 and generalized it.
toRomanNumeral :: Integer -> String
toRomanNumeral x 
	|x == 0 = ""
	| x > 0 = b ++ toRomanNumeral (x - a)
		where (a, b) = head $ filter ((<= x) . fst) cMap