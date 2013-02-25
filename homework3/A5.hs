import System.Environment
import Data.Char 

--type Parser a = String -> [(a,String)]

--data Grammar = Operator | Digit | Letter | Whitespace | Error

--------------- TOKENIZER ----------------------
type Token = (String,String)

-- data Tree a = EmptyTree | node a (Tree a) (Tree a) deriving (Show, read, Eq)


-- TO DO: variable names, white space, keywords, numbers, variables, operators
isOperator :: String -> Bool
isOperator x 
    | x == "+" = True
    | x == "-" = True
    | x == "*" = True
    | x == "/" = True
    | otherwise = False

-- Re-inventing the wheel because we want strings, not chars.
isADigit :: String -> Bool
isADigit c = c >= "0" && c <= "9"


getToken :: String -> Token
getToken x
    | isADigit x             =       ("Digit",x)
    | x == " "               =       ("Whitespace",x)
    | isOperator x           =       ("Operator",x)
    | isLetter (head x)      =       ("Letter",x)
    | (x == "E" || x == "e") =       ("Exp",x)
    | x == "="               =       ("Equals",x)
    | x == "."               =       ("Period",x)
    | x == "("               =       ("Open",x)
    | x == ")"               =       ("Close",x)
    | x == ";"               =       ("Semicolon",x)
    | otherwise              =       ("Error",x)

tokenize :: String -> [Token]
tokenize (x:xs) = (getToken [x]) : (tokenize xs)
tokenize [] = []

-- removes redundant whitespace

-- finds key tokens
isKey :: Token -> Token -> Token -> Bool
isKey a b c 
    | (snd a == "v") && (snd b == "a") && (snd c == "r") = True
    | (snd a == "d") && (snd b == "e") && (snd c == "f") = True
    | otherwise = False

-- constructs a key token
makeKey :: Token -> Token -> Token -> Token
makeKey a b c
    | (snd a == "v") = ("Key", "v")
    | (snd a == "d") = ("Key", "d")

-- finds keywords and reduces them to atomic tokens
keywords :: [Token] -> [Token]
keywords (a:b:c:ds) 
    | (isKey a b c) = (makeKey a b c) : keywords ds
    | otherwise = a : (keywords (b:c:ds))
keywords a = a

--tokenToFloat :: [Token] -> Float
--tokenToFloat (a:b:c:ds)
--	| (fst a == "Digit" && fst b == "Period" && fst c == "Digit") = (read ((snd a) ++ (snd b) ++ (snd c)) :: Float)
--tokenToFloat [] = 0.0

digitToDecimal :: [Token] -> [Token]
digitToDecimal (a:b:cs) 
	|  ((fst a == "Digit") && (fst b == "Digit")) = digitToDecimal( ("Digit", (snd a) ++ (snd b)) : cs )
	| otherwise = a: (digitToDecimal (b:cs))
digitToDecimal a = a

isPlusOrMinus :: Token -> Bool
isPlusOrMinus a
	| (snd a == "+" || snd a == "-") = True
    | otherwise = False

-- Good
decimalToExponent :: [Token] -> [Token]
decimalToExponent (a:b:c:ds)
    | ((fst a == "Letter") && (isPlusOrMinus b) && (fst c == "Digit")) = decimalToExponent ( ("Exponent", (snd a) ++ (snd b) ++ (snd c) ) : ds )
    | otherwise = a: (decimalToExponent (b:c:ds))
decimalToExponent a = a

-- works
floatMagic :: Token -> Token -> Token -> Token -> Bool
floatMagic a b c d
	| ( (fst a == "Digit") && (fst b == "Period") && (fst c == "Digit") && (fst d == "Exponent") ) = True
	| otherwise = False

-- also works
floatMagic2 :: Token -> Token -> Token -> Bool
floatMagic2 a b c
	| ( (fst a == "Digit") && (fst b == "Period") && (fst c == "Digit") ) = True
	| otherwise = False

exponentToFloat :: [Token] -> [Token]
exponentToFloat (a:b:c:d:es)
	| ( floatMagic a b c d) = exponentToFloat( ("Float", (snd a) ++ (snd b) ++ (snd c) ++ (snd d) ) : es ) 
exponentToFloat (a:b:c:ds)
    |( floatMagic2 a b c ) = exponentToFloat( ("Float", (snd a) ++ (snd b) ++ (snd c)) : ds ) 
exponentToFloat (a:b:cs)
	| ( (fst a == "Digit") && (fst b == "Exponent") ) = exponentToFloat(("Float", (snd a) ++ (snd b) ) : cs )
exponentToFloat (a:bs)
	| (fst a == "Digit") = ("Float", (snd a)) : bs 
	| otherwise = a : (exponentToFloat (bs))
exponentToFloat a = a

isLetNum :: Char -> Bool
isLetNum a = isLetter a || isDigit a

andl :: [Bool] -> Bool
andl [] = True
andl (x:xs) = x && and xs

isVariable :: String -> Bool
isVariable (a:bs) = (isLetter  a) && andl (map isLetNum bs)
isVariable a = isLetter (head a)

	

-- letterToVariable :: [Token] -> [Token]
-- letterToVariable (a:b:s)
	-- | isVariable(a:b) = letterToVariable(("Variable", 

tokenMagic :: String -> [Token]
tokenMagic a = exponentToFloat $ decimalToExponent $ digitToDecimal $ tokenize a
---------------------------------------------------------------------

---------------EVALUATOR---------------------


-------------------ABSTRACT SYNTAX TREE------------------------------

--eval :: [Token] -> Tree
--eval 

tokenToDigit :: Token -> Int
tokenToDigit a
	| (fst a == "Digit") = digitToInt(head(snd a)) :: Int 

add :: [Token] -> Maybe Int
add [a] = Just (tokenToDigit a)
add [a,b] = Nothing
--add [a,b,c] 

--   | (fst a == "Digit" && snd b == '+' && fst c == "Digit") = ((read (fst a) :: Int) + (read (fst c) :: Int))
--   | otherwise = 0

 
--move input to output
echo :: IO ()
echo = do
       line <- getLine
       if line == "quit" then
           do putStrLn "Exiting..."
       else
           do
           putStrLn line
           echo

main :: IO ()
main = do echo