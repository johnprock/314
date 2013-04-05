import qualified Data.ByteString as B
import System.Environment

data State = State {pos::Int, cp::Int, cpmax::Int, cont::B.ByteString}
-- pos:   position in byteString
-- cp:    position in constant pool
-- cpmax: size of constant pool
-- cont:  contents of class file


-- check if a bytestring starts with a magic constant
isMagic :: B.ByteString -> Bool
isMagic b = ca && fe && ba && be
  where
    ca = fromIntegral (B.index b 0) == 202
    fe = fromIntegral (B.index b 1) == 254
    ba = fromIntegral (B.index b 2) == 186
    be = fromIntegral (B.index b 3) == 190

-- process the magic constant
procMagic :: State -> (State, String)
procMagic s = let new = State {pos = 4, cp = -1, cpmax = -1, cont = (cont s)}
              in if isMagic (cont s) 
                 then (new, "CAFE!!!!!!")
                 else (new, "Failed to process magic constant")





-- top level function
-- recurs through entire class file
-- processes each byte according to state
dissect :: State -> String
dissect state 
  | pos state == 0 = snd (procMagic state) ++ dissect(fst (procMagic state))
  | otherwise = "\nFinished.\n"


-- get bytestring
-- process bytestring
-- print results
main :: IO()
main = do

--  filename <- getLine
  contents <- B.readFile "ClassFileReporter.class" -- fix this
  putStrLn $ dissect (State {pos = 0, cp = -1, cpmax = -1, cont = contents})
