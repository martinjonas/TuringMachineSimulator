import TuringMachine
import TuringMachineParser
import System.Environment

simulateTMOnWord :: TM String Char -> String -> IO ()
simulateTMOnWord tm word = do
  let c = run tm word
  print "Final configuration: "
  putStrLn (show c)
  putStrLn (if isAcceptingState tm (state c) then "ACCEPT" else "REJECT")

simulateTMOnWordSteps :: TM String Char -> String -> Int -> IO ()
simulateTMOnWordSteps tm word steps = do
  let finalConf = runSteps tm word steps
  case finalConf of
   Nothing -> putStrLn "Exceeded step limit"
   Just c -> do
     print "Final configuration: "
     putStrLn (show c)
     putStrLn (if isAcceptingState tm (state c) then "ACCEPT" else "REJECT")

main :: IO ()
main = do
  args <- getArgs
  let fileName = args !! 0
  let word = args !! 1
  tmCode <- readFile fileName
  let parseResult = parseTM tmCode
  case parseResult of
   Left _ -> print "Parse error"
   Right tm -> (if (length args > 2) then simulateTMOnWordSteps tm word (read $ args !! 2) else simulateTMOnWord tm word)
     
  
