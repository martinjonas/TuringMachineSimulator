import TuringMachine
import TuringMachineParser
import Control.Monad
import Data.List
import System.Environment

getAllWordsOfLengthAtMost :: String -> Int -> [String]
getAllWordsOfLengthAtMost chars wordLength = concatMap (\l -> mapM (const chars) (replicate l 1)) [0..wordLength]

correct :: String -> Bool
correct "" = False
correct input = correctForm && correctCount
  where
    groups = group input
    correctForm = (length groups == 1 && all (=='b') (head groups)) || ((length groups == 2) && (all (=='a') (groups !! 0)) && (all (=='b') (groups !! 1)))
    correctCount = (length groups == 1) || (mod (length $ groups !! 0) (length $ groups !! 1) == 0)

getResult :: TM String Char -> String -> Int -> Maybe Bool
getResult tm word steps = case runSteps tm word steps of
  Nothing -> Nothing
  Just c -> Just (isAcceptingState tm (state c))
    
main :: IO ()
main = do
  args <- getArgs
  let [filename, steps, chars, wordLength] = args
  tmCode <- readFile filename
  let parseResult = parseTM tmCode
  case parseResult of
   Left err -> print err
   Right tm -> do
     let words = getAllWordsOfLengthAtMost chars (read wordLength)
     let results = map (\w -> (w, getResult tm w (read steps) == Just (correct w))) words
     let (correctResults, incorrectResults) = partition snd results
     putStrLn ("Correct: " ++ show (length correctResults))
     putStrLn ("Incorrect: " ++ show (length incorrectResults))
     if (not (null incorrectResults)) then print (map fst incorrectResults) else return ()
