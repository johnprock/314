import System.Environment
import Data.Char 

--type Parser a = String -> [(a,String)]

--data Grammar = Operator | Digit | Letter | Whitespace | Error

--------------- TOKENIZER ----------------------
type Token = (String,String)

-- data Tree a = EmptyTree | node a (Tree a) (Tree a) deriving (Show, read, Eq)


-- TO DO: variable names, white space, keywords, numbers, variables, operators
isOperator :: Char -> Bool
isOperator x 
    | x == '+' = True
    | x == '-' = True
    | x == '*' = True
    | x == '/' = True
    | otherwise = False

getToken :: Char -> Token
getToken x
    | isDigit x =              ("Digit",x)
    | x == ' ' =               ("Whitespace",x)
    | isOperator x =           ("Operator",x)
    | isLetter x =             ("Letter",x)
    | x == '=' =               ("Equals",x)
    | x == '.' =               ("Period",x)
    | x == '(' =               ("Open",x)
    | x == ')' =               ("Close",x)
    | (x == 'E' || x == 'e') = ("Exp",x)
    | x == ';' =               ("Semicolon",x)
    | otherwise =              ("Error",x)

tokenize :: String -> [Token]
tokenize (x:xs) = (getToken x) : (tokenize xs)
tokenize [] = []

-- removes redundant whitespace

-- finds key tokens
isKey :: Token -> Token -> Token -> Bool
isKey a b c 
    | (snd a == 'v') && (snd b == 'a') && (snd c == 'r') = True
    | (snd a == 'd') && (snd b == 'e') && (snd c == 'f') = True
    | otherwise = False

-- constructs a key token
makeKey :: Token -> Token -> Token -> Token
makeKey a b c
    | (snd a == 'v') = ("Key", 'v')
    | (snd a == 'd') = ("Key", 'd')

-- finds keywords and reduces them to atomic tokens
keywords :: [Token] -> [Token]
keywords (a:b:c:ds) 
    | (isKey a b c) = (makeKey a b c) : keywords ds
    | otherwise = a : (keywords (b:c:ds))
keywords a = a

tokenToFloat :: [
---------------------------------------------------------------------


-------------------ABSTRACT SYNTAX TREE------------------------------

--eval :: [Token] -> Tree
--eval 

tokenToDigit :: Token -> Int
tokenToDigit a
	| (fst a == "Digit") = digitToInt(snd a) :: Int 

add :: [Token] -> Maybe Int
add [a] = Just (tokenToDigit a)
add [a,b] = Nothing
add [a,b,c] 

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