-- Patrick Rock
-- Kalil Armstrong

import qualified Data.ByteString as B
import System.Environment
import Codec.Binary.UTF8.String as S

-- This program passes a data structure recursively and updates it
-- with respect to how far it is in the bytestream.
-- It iterates through each section of Java bytecode and stores it in the
-- Structure and then prints it. We ran into an issue with iterating through
-- The bytecode (see the proc function) during the tag section, but had
-- accomodated for the variable size of the first tag. Utility functions
-- for each field would follow for the constant pool.
-- Currently, it gets the major and minor version number and the size of the constant pool

-------- DATA STRUCTURES --------------
-- pos cp cpmax cpsize isize fsize
data State = State {intData::[Int], cont::B.ByteString, cpData::[String]}
-- pos:    position in byteString
-- cp:     position in constant pool
-- cpmax:  size of constant pool
-- cont:   contents of class file
-- cpData: contents of constant pool


--------- HELPER FUCNTIONS --------------


getPos :: State -> Int
getPos s = (intData s)!!0

getcp s = (intData s)!!1

getcpmax s = (intData s)!!2

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

-- get a 16 bit integer from the byte stream
get16 :: State -> Int
get16 s = wordToInt (B.index (cont s) ((getPos s))) (B.index (cont s) ((getPos s)+1))

-- determine the tag value
tag :: State -> Int
tag s = fromIntegral $ B.index (cont s) ((intData s)!!0 ) 

-- get a sting of size size from a state state
getString :: State -> Int -> String
getString state size = "lol"--S.decode $ B.unpack $ B.take size (cont state) 
---------- PROCESSOR FUNCTIONS ---------------

-- process the magic constant
procMagic :: State -> (State, String)
procMagic s = let new = State {intData = [4, -1, -1, 0, 0, 0],  cont = (cont s), cpData = []}

              in if isMagic (cont s) 
                 then (new, "CAFEBABE!!!!!!\n")
                 else (new, "Failed to process magic constant\n")

-- process the major version
procMinor :: State -> (State, String)
procMinor s = (new, "Minor Version: " ++ minor ++ "\n")
	where
		_minor = wordToInt (B.index (cont s) 4) (B.index (cont s) 5)
                minor  = show _minor
                new    = State {intData = [6, -1,-1, 0, 0, 0], cont = (cont s),
                cpData = []}


-- process the minor version
procMajor :: State -> (State, String)
procMajor s = (new, "Major Version: " ++ (show major) ++ "\n")
	where
		majora = fromIntegral (B.index (cont s) 6)
		majorb = fromIntegral(B.index (cont s) 7)
		major  = wordToInt majora majorb
                new    = State {intData = [8,-1,-1,0,0,0], cont = (cont s),
                cpData = []}

-- process the constant pool count
procCPCnt s = (new, "Constant Pool Count: " ++ (show count) ++ "\n")
        where
          count = wordToInt (B.index (cont s) 8) (B.index (cont s) 9)
          new   = State [10, 0, (count-1), 0, 0, 0]  (cont s) []

-- process the constant pool
procCP :: State -> (State, String)
procCP s 
  | tag s == 1   = procCP $ procT1 s   
  | tag s == 3   = procCP $ procT3 s
  | tag s == 4   = procCP $ procT4 s
  | tag s == 5   = procCP $ procT5 s
  | tag s == 6   = procCP $ procT6 s
  | tag s == 7   = procCP $ procT7 s
  | tag s == 8   = procCP $ procT8 s
  | tag s == 9   = procCP $ procT9 s
  | tag s == 10  = procCP $ procT10 s
  | tag s == 11  = procCP $ procT11 s
  | tag s == 12  = procCP $ procT12 s
  | ((intData s)!!1 ) == ((intData s)!!2 ) = (s, "lool")
  | otherwise = (s, "ROFL")


inc:: State -> Int -> State
inc s i = State{ intData = [((intData s)!!0) + i,((intData s)!!1 ) +1,((intData s)!!2),0,0,0], cont = (cont s), cpData = (cpData s) }   

inc2:: State -> Int -> State
inc2 s i = State{ intData = [((intData s)!!0) + i,((intData s)!!1 ) ,((intData s)!!2) ,0,0,0], cont = (cont s), cpData = (cpData s) }   


-- process a constant pool entry with tag 1
--procT1 :: State -> State
--procT1 s = inc s (size+3)
--  where size = get16 s

procT1 s = State{ intData = [((intData s)!!0)+size+3,((intData s)!!1 ) +1,((intData s)!!2 ),0,0,0],
cont = (cont s), cpData = string : (cpData s) }
        where 
          size = get16 s
          string = getString (inc2 s 3) size

procT3 :: State -> State
procT3 s = inc s 5
--State[ pos = (pos s)+size+3, cp = cp(s) + 1, cpmax = (cpmax s), cont
-- = (cont s), cpData = string : (cpData s) }
--        where
--          size = 
--          string = getString (State ((pos s)

procT4 :: State -> State
procT4 s = inc s 5

procT5 :: State -> State
procT5 s = inc s 9

procT6 :: State -> State
procT6 s = inc s 9

procT7 :: State -> State
procT7 s = inc s 3

procT8 :: State -> State
procT8 s = inc s 3

procT9 :: State -> State
procT9 s = inc s 5

procT10 :: State -> State
procT10 s = inc s 5

procT11 :: State -> State
procT11 s = inc s 5

procT12 :: State -> State
procT12 s = inc s 5
        

-- process the access flags

-- process the this class index

-- process the super class index

-- process the interface count

-- process the interface

-- process the field count

-- process the field table

-- process the method count

-- process the method table

-- process the attribute count

-- process the attribute table


----------- RECURSIVE STRUCTURE ------------

-- calls procCP recursively
-- cprecur :: (State -> (State, String)) -> State -> String

-- calls processor functions recursively
drecur :: (State -> (State, String)) -> State -> String
drecur func s = snd (func s) ++ dissect(fst (func s))

------------ DISSECT ----------------

-- top level function
-- recurs through entire class file
-- processes each byte according to state
dissect :: State -> String
dissect state 
  | getPos state == 0 =  drecur procMagic state 
  | getPos state == 4 =  drecur procMajor state
  | getPos state == 6 =  drecur procMinor state
  | getPos state == 8 =  drecur procCPCnt state
  | getPos state == 10 = drecur procCP    state
  | otherwise = "\nFinished.\n"


-- get bytestring
-- process bytestring
-- print results
main :: IO()
main = do

  contents <- B.readFile "ClassFileReporter.class" -- fix this
  putStrLn $ dissect (State {intData = [0,-1,-1,0,0,0], cont = contents,
  cpData = []})
