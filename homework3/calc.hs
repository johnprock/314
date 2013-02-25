import System.Environment
import Data.Char

data Token = Number Float --used to build a token stream 
           | Plus
           | Minus
           | Mult
           | Div
           | Variable String
           | Open
           | Close
           | Period
           | Var
           | Def
           deriving (Show)


isAlphanum :: Char -> Bool
isAlphanum a = isAlpha a || isDigit a

nextToken :: String -> [(Token, String)] --consumes one token
nextToken [] = []
nextToken (a:ds) --these patterns match simple tokens
    | a == '+' = [(Plus, ds)]
    | a == '-' = [(Minus, ds)]
    | a == '*' = [(Mult, ds)]
    | a == '/' = [(Div, ds)]
    | a == '(' = [(Open, ds)]
    | a == ')' = [(Close, ds)]
    | a == '.' = [(Period, ds)]
    | a == ' ' = nextToken (ds) --ignore whitespace
nextToken (a:b:c:ds)
    | isVar = [(Var, ds)]
    | isDef = [(Def, ds)]
    where
      isVar = a=='v' && b=='a' && c=='r'
      isDef = a=='d' && b=='e' && c=='f'
nextToken (a:bs) --match numbers and variables
    | isAlpha a = [((Variable $ [a] ++ v), r1)]
    | isDigit a = [(Number (read n), r2)]
    where 
      v = takeWhile isAlphanum bs
      r1 = dropWhile isAlphanum bs
      r2 = dropWhile isDigit bs
      n = a : (takeWhile isDigit bs)


tokenize :: String -> [Token] --converts a string to a token stream
tokenize [] = []
tokenize s = (fst next):(tokenize $ snd next)
    where next = head (nextToken s)




main :: IO ()
main = do putStrLn "Hi"
