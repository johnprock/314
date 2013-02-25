import System.Environment

newtype Parser a = Parser (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (Parser p) inp = p inp

instance Monad Parser where
   return v = Parser (\inp -> [(v,inp)])
   p >>= f =  Parser (\inp -> case parse p inp of
                         []        -> []
                         [(v,out)] -> parse (f v) out)

data Grammar = Operator | Digit 

                   

--copy input to output
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