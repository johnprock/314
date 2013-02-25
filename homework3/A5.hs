import System.Environment

type Parser a = String -> [(a,String)]

data Grammar = Operator | Digit             

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