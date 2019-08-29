import Data.Word

-- The tape is represented as a head (the middle element), and a list of elements before and after the head
-- XXX(Isaac): The before list is back-to-front (i.e. the first item is the element before head, not the last)
type Tape = ([Word8],Word8,[Word8])

data Program = Program {tape    :: Tape,
                        input   :: [Word8],
                        output  :: String
                       } deriving (Show)

-- Runs an operation `op` on index `n` of the tape
runTapeOp :: (Word8 -> Word8) -> Tape -> Tape
runTapeOp op (before,point,after) = (before,(op point),after)

moveHeadLeft :: Tape -> Tape
moveHeadLeft (before,point,after) = ((tail before),(head before),(point:after))

moveHeadRight :: Tape -> Tape
moveHeadRight (before,point,(next:newAfter)) = ((point:before),next,newAfter)

--- TODO: we don't support [ or ] yet
step :: Program -> Char -> Program
step p '>' = Program (moveHeadRight $ tape p)                         (input p)         (output p)
step p '<' = Program (moveHeadLeft $ tape p)                          (input p)         (output p)
step p '+' = Program (runTapeOp (+1) (tape p))                        (input p)         (output p)
step p '-' = Program (runTapeOp (subtract 1) (tape p))                (input p)         (output p)
step p '.' = Program (tape p)                                         (input p)         ((output p)++(show $ (let (before,point,after) = (tape p) in point)))
step p ',' = Program (runTapeOp (const $ head $ input p) (tape p))    (init $ input p)  (output p)
--step p '[' = Program (tape p) (input p) (output p)
--step p ']' = Program (tape p) (input p) (output p)
step p c = error ("Invalid character: "++[c])

run :: String -> [Word8] -> Program
run source inputTape = foldl step (Program ((repeat 0),0,(repeat 0)) inputTape "") source

prompt :: String -> IO String
prompt message = do
  putStr message
  getLine

main :: IO ()
main = do
  source <- prompt "> "
  inputStr <- (if (',' `elem` source) then (prompt "Input> ")
                                      else (return ""))
  putStrLn $ output $ run source (map (toEnum . fromEnum) inputStr)
