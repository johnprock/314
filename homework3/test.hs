newtype Parser a = Parser (String -> [(a,String)])

instance Monad Parser where
  return a = Parser (\cs -> [(a,cs)])
  p >>= f = Parser (\cs -> concat [parse (f a) cs' |
                             (a,cs') <- parse p cs])