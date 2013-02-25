import System.Environment
import Data.Char 

type Parser a = String -> [(a,String)]

--data Grammar = Operator | Digit | Letter | Whitespace | Error

isOperator :: Char -> Bool
isOperator x 
    | x == '+' = True
    | x == '-' = True
    | x == '*' = True
    | x == '/' = True
    | otherwise = False

getToken :: String -> (String,Char)
getToken (x:xs) 
    | isDigit x = ("Digit",x)
    | x == ' ' = ("Whitespace",x)
    | isOperator x = ("Operator",x)
    | isLetter x = ("Letter",x)
  
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