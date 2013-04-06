import qualified Data.ByteString as B
import System.Environment



-------- DATA STRUCTURES --------------

data State = State {pos::Int, cp::Int, cpmax::Int, cont::B.ByteString}
-- pos:   position in byteString
-- cp:    position in constant pool
-- cpmax: size of constant pool
-- cont:  contents of class file


--------- HELPER FUCNTIONS --------------

-- check if a bytestring starts with a magic constant
isMagic :: B.ByteString -> Bool
isMagic b = ca && fe && ba && be
  where
    ca = fromIntegral (B.index b 0) == 202
    fe = fromIntegral (B.index b 1) == 254
    ba = fromIntegral (B.index b 2) == 186
    be = fromIntegral (B.index b 3) == 190


-- convert two Word8 to a 16 bit integer
wordToInt _a _b = b + a*256  
  where
    a = fromIntegral(_a)
    b = fromIntegral(_b)


---------- PROCESSOR FUNCTIONS ---------------

procMinor:: State -> (State, String)
procMinor s = (new, "Minor Version: " ++ minor ++ "\n")
	where
		_minor = wordToInt (B.index (cont s) 4) (B.index (cont s) 5)
                minor  = show _minor
                new    = State {pos = 6, cp = -1, cpmax = -1, cont = (cont s)}

procMajor:: State -> (State, String)
procMajor s = (new, "Major Version: " ++ (show major) ++ "\n")
	where
		majora = fromIntegral (B.index (cont s) 6)
		majorb = fromIntegral(B.index (cont s) 7)
		major  = wordToInt majora majorb
                new    = State {pos = 8, cp = -1, cpmax = -1, cont = (cont s)}

-- process the magic constant
procMagic :: State -> (State, String)
procMagic s = let new = State {pos = 4, cp = -1, cpmax = -1, cont = (cont s)}
              in if isMagic (cont s) 
                 then (new, "CAFE!!!!!!\n")
                 else (new, "Failed to process magic constant\n")



------------ DISSECT ----------------

-- top level function
-- recurs through entire class file
-- processes each byte according to state
dissect :: State -> String
dissect state 
  | pos state == 0 = snd (procMagic state) ++ dissect(fst (procMagic state))
  | pos state == 4 = snd (procMinor state) ++ dissect(fst (procMinor state))
  | pos state == 6 = snd (procMajor state) ++ dissect(fst (procMajor state))
  | otherwise = "\nFinished.\n"


-- get bytestring
-- process bytestring
-- print results
main :: IO()
main = do

  contents <- B.readFile "ClassFileReporter.class" -- fix this
  putStrLn $ dissect (State {pos = 0, cp = -1, cpmax = -1, cont = contents})
