import System.Environment
import Data.Char

data Kind = Number --used to build a token stream 
           | Plus
           | Minus
           | Mult
           | Div
           | Equals
           | Variable
           | Open
           | Close
           | Period
           | Var
           | Def
           | Semicolon
		   | NULL
           deriving (Show,Eq)
		   
data Token = Token { kind :: Kind, 
                     value :: String } deriving (Show,Eq)

data Statement = Statement{dec::Declaration,
						   expr1::Expression} deriving(Show,Eq) 

data Declaration = Declaration
                   {keyw::Kind,
					headOrVar::Kind,
					eq::Kind,
					expressation::Kind
				   } deriving(Show,Eq)

data Head = Head 
            { v1::Token
			} deriving(Show,Eq)

data Expression = Expression
                  {express::Expression, 
				  t1::Term
				  } deriving(Show,Eq)
				  
data Term = Term 
            { p1::Primary,
			  term::Term,
			  op::Token
			} deriving(Show,Eq)
          
data Primary = Primary 
               { fp::FloatingPoint,
			     var::Kind,
				 primexp::Expression } deriving (Show,Eq)


data FloatingPoint = FloatingPoint
                     { first::Token,
					   second::Token,
					   expr::Exponent } deriving (Show,Eq)

data Exponent = Exponent { sign::Kind,
						   num::Token } deriving (Show,Eq)

----------------------PARSER---------------------------------------	  

type Parser a =  [Token] -> [(a, [Token])]

parse :: Parser a -> [Token] -> [(a,[Token])]
parse p inp = p inp

pbind :: Parser a -> (a -> Parser b) -> Parser b --sequence operator
pbind p f = \inp -> case parse p inp of
                    [] -> []
                    [(v,out)] -> parse (f v) out
					
pchoose :: Parser a -> Parser a -> Parser a --tries p then tries q
pchoose p q = \inp -> case parse p inp of
                      [] -> parse q inp
                      [(v,out)] -> [(v,out)]
					  
preturn :: a -> Parser a
preturn v = \inp -> [(v,inp)]

item :: Parser Token --gets one token from the token stream
item = \inp -> case inp of
               [] -> []
               (x:xs) -> [(x,xs)]

failure :: Parser a
failure = \inp -> []
			   
           


------------------------------------------------

nullt = Token {kind = NULL, value = ""}
nullexp = Exponent{ sign = NULL, num = nullt }		
-- nullterm = Expression{ express = nullexp}
 
--------------------------------------
parseExponent :: [Token] -> [(Exponent, [Token])] -- parses an exponent and returns unconsumed values
parseExponent (a:b:c:ds) 
            | isExp && (kind b == Plus) = [(Exponent{ sign = Plus, num = c }, ds)]
            | isExp && (kind b == Minus) = [(Exponent{ sign = Minus, num = c }, ds)]
			| otherwise = []
			where 
			  isExp = (value a == "e"||value a == "E") && (kind c == Number)
parseExponent a = []
----------------------------------------
parseFloat :: [Token] -> [(FloatingPoint, [Token])]
parseFloat (a:b:c:ds) 
        | (isFloat1 && parseExponent ds /= []) = [(FloatingPoint { first = a, second = c, expr = fst $ head $ parseExponent ds }, snd $ head $ parseExponent ds)] --compiles
		| (isFloat2 && parseExponent (b:c:ds) /= []) = [(FloatingPoint { first = a, second = nullt, expr = fst $ head $ parseExponent ds }, snd $ head $ parseExponent ds )] 
		|  isFloat1 = [(FloatingPoint { first = a, second = c, expr = Exponent{ sign = NULL, num = nullt }}, ds)]
			where 
			  isFloat1 = kind a == Number && kind b == Period && kind c == Number
			  isFloat2 = (kind a == Number)
parseFloat (a:bs) 
        | kind a == Number = [(FloatingPoint { first = a, second = nullt, expr = Exponent{ sign = NULL, num = nullt }}, bs)]
parseFloat a = []
----------------------------------------------------------
-- parsePrimary :: [Token] -> [(Primary, [Token])]
-- parsePrimary (a:b:c:ds) 	 
			-- | (isPrimary && parseFloat ds /= []) = [(Primary { fp = fst a, var = nullt, primexp = nullexp}, ds)]
			-- | (isPrimary2 && parseFloat ds /= []) = [(Primary { fp = 0, var = a, primexp = nullexp}, ds)]

			-- where
			  -- isPrimary  = kind a == FloatingPoint
			  -- isPrimary2 = kind a == Variable
			  -- isPrimary3 = kind a == Variable && kind b == Expression
			  -- isPrimary4 = kind a == Expression

----------------------------------------------		
		
-- parseTerm :: [Token] -> [(Term, [Token])]
-- parseTerm  (a:b:cs)
-- parseTerm a
    -- | (parsePrimary /= []) = [(Term {p1=fst$head$parsePrimary a}, snd$head$parsePrimary a)]
	-- | otherwise = termRecur a
	


	
	
--parseExpression [Token] -> [(Expression, [Token])]

--parseHead [Token] -> [(Head, [Token])]

--parseDeclaration [Token] -> [(Head, [Token])]

--parseStatement [Token] -> [(Statement, [Token])]
			  
			  
-------------------------TOKENIZER-------------------------------------
isAlphanum :: Char -> Bool
isAlphanum a = isAlpha a || isDigit a

nextToken :: String -> [(Token, String)] --consumes one token
nextToken [] = []
nextToken (a:ds) --these patterns match simple tokens
    | a == '+' = [(Token { kind = Plus,      value = "+" }, ds)]
    | a == '-' = [(Token { kind = Minus,     value = "-" }, ds)]
    | a == '*' = [(Token { kind = Mult,      value = "*"}, ds)]
    | a == '/' = [(Token { kind = Div,       value = "/"}, ds)]
    | a == '=' = [(Token { kind = Equals,    value = "="   }, ds)]
    | a == '(' = [(Token { kind = Open,      value = "("     }, ds)]
    | a == ')' = [(Token { kind = Close,     value = ")"    }, ds)]
    | a == '.' = [(Token { kind = Period,    value = "."   }, ds)]
    | a == ';' = [(Token { kind = Semicolon, value = ";"} , ds)]
    | a == ' ' = nextToken (ds) --ignore whitespace
nextToken (a:b:c:ds)
    | isVar = [(Token { kind = Var, value = "var"}, ds)]
    | isDef = [(Token { kind = Def, value = "def"}, ds)]
    where
      isVar = a=='v' && b=='a' && c=='r'
      isDef = a=='d' && b=='e' && c=='f'
nextToken (a:bs) --match numbers and variables
    | isAlpha a = [(Token { kind = Variable, value = [a] ++ v}, r1)]
    | isDigit a = [(Token { kind = Number,  value = n}, r2)]
    where 
      v = takeWhile isAlphanum bs
      r1 = dropWhile isAlphanum bs
      r2 = dropWhile isDigit bs
      n = a : (takeWhile isDigit bs)

tokenize :: String -> [Token] --converts a string to a token stream
tokenize [] = []
tokenize s = (fst next):(tokenize $ snd next)
    where next = head (nextToken s)
-------------------------------------------------------------------------

main :: IO ()
main = do putStrLn "Hi"
