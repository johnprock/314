import qualified Data.ByteString as B


data State = State {pos::Int, cp::Int, cpmax::Int, cont::B.ByteString}



-- check if a bytestring starts with a magic constant
isMagic :: B.ByteString -> Bool
isMagic b = ca && fe && ba && be
  where
    ca = (B.index b 0) == 11001010
    fe = (B.index b 1) == 11111110
    ba = (B.index b 2) == 10111010
    be = (B.index b 3) == 10111110

-- process the magic constant
procMagic :: State -> (State, String)
procMagic s = if isMagic (cont s) 
                then (s, "CAFE")
                else (s, "fail")

dissect :: State -> Int
dissect x = 5

main :: IO()
main = do
  filename <- getLine
  contents <- B.readFile filename
  B.putStr contents
