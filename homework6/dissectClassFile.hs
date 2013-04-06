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
	
--getMinorVersion:: B.ByteString -> Integer
getMinorVersion b = minor
	where
		minora = fromIntegral(B.index b 4)
		minorb =  fromIntegral(B.index b 5)
		minor = minora + (minorb*(2^8))

--getMajorVersion::B.ByteString -> Integer
getMajorVersion b = major
	where
		majora =fromIntegral (B.index b 6)
		majorb = fromIntegral(B.index b 7)
		major = majora + (majorb*(2^8))

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

  filename <- getLine
  contents <- B.readFile filename --"ClassFileReporter.class" -- fix this
  putStrLn $ dissect (State {pos = 0, cp = -1, cpmax = -1, cont = contents})
