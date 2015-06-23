module TuringMachineParser where

import TuringMachine
import Text.ParserCombinators.Parsec
import Control.Applicative hiding (many, (<|>))
import Data.Map hiding (map)
--import Control.Monad

data Rule = Rule (String, Char) (String, Char, Movement)

ruleToPair :: Rule -> ((String, Char), (String, Char, Movement))
ruleToPair (Rule preImg img) = (preImg, img)

parentheses :: Parser a -> Parser a
parentheses = between (char '(') (char ')')

stateParser :: Parser String
stateParser = spaces >> between (char '"') (char '"') (many $ noneOf "\"") <* spaces

symbolParser :: Parser Char
symbolParser = spaces >> between (char '\'') (char '\'') anyChar <* spaces

movementParser :: Parser Movement
movementParser = spaces >> choice [MoveLeft <$ char 'l', MoveLeft <$ char 'L', MoveRight <$ char 'r', MoveRight <$ char 'R'] <* spaces

preimageParser :: Parser (String, Char)
preimageParser = parentheses ((,) <$> (stateParser <* char ',') <*> symbolParser)

imageParser :: Parser (String, Char, Movement)
imageParser = parentheses ((,,) <$> (stateParser <* char ',') <*> (symbolParser <* char ',') <*> movementParser)

ruleParser :: Parser Rule
ruleParser =  Rule <$> (preimageParser <* char '=') <*> (imageParser)

rulesParser :: Parser [Rule]
rulesParser = ruleParser `sepEndBy` string "\n"

tmParser :: Parser (TM String Char)
tmParser = fmap (\rules -> TM '|' '_' (rulesToTransitionFunction rules) "q_0" "q_acc" "q_rej") rulesParser

rulesToTransitionFunction :: [Rule] -> Map (String, Char) (String, Char, Movement)
rulesToTransitionFunction = fromList . map ruleToPair

parseTM :: String -> Either ParseError (TM String Char)
parseTM input = parse tmParser "(parse error)" input
