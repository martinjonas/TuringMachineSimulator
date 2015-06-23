module TuringMachine where

import Data.Map hiding (filter)
import Data.Maybe

data Movement = MoveLeft | MoveRight deriving Show

data TM q s  = TM
  { leftMarker :: s,
    blank :: s,
    transitionFunction :: Map (q, s) (q, s, Movement),
    initial :: q,
    accepting :: q,
    rejecting :: q} deriving Show

newtype Tape s = Tape (Map Int s) deriving Show

data Configuration q s = Conf
  { state :: q,
    tape :: Tape s,
    position :: Int} deriving Show

getTapePosition :: TM q s -> Tape s -> Int -> s
getTapePosition (TM _ b _ _ _ _) (Tape t) pos =
  case Data.Map.lookup pos t of
    Nothing -> b
    Just s' -> s'

modifyTape :: Tape s -> Int -> s -> Tape s
modifyTape (Tape t) pos new = Tape (alter (const (Just new)) pos t)

step :: (Eq q, Ord q, Ord s) => TM q s -> Configuration q s -> Configuration q s
step tm@(TM _ _ f _ acc rej) (Conf current tape pos)
  | current == acc || current == rej = Conf current tape pos
  | otherwise = case Data.Map.lookup (current, (getTapePosition tm tape pos)) f of
                 Nothing -> Conf rej tape pos
                 Just (q', s', MoveRight) -> Conf q' (modifyTape tape pos s') (pos + 1)
                 Just (q', s', MoveLeft) -> if pos == 0 then Conf current tape pos else Conf q' (modifyTape tape pos s') (pos - 1)
    
getRunFromConf :: (Eq q, Ord q, Ord s) => TM q s -> Configuration q s -> [Configuration q s]
getRunFromConf tm conf = iterate (step tm) conf

isHaltingState :: (Eq q) => TM q s -> q -> Bool
isHaltingState (TM _ _ _ _ acc rej) s = s == acc || s == rej

isAcceptingState :: (Eq q) => TM q s -> q -> Bool
isAcceptingState (TM _ _ _ _ acc _) s = s == acc

getHaltingConf :: (Eq q) => TM q s -> [Configuration q s] -> Maybe (Configuration q s)
getHaltingConf tm = listToMaybe . filter (isHaltingState tm . state)

initialConf :: TM q s -> [s] -> Configuration q s
initialConf (TM l _ _ init _ _) word = Conf init (Tape $ Data.Map.fromList ((0,l):zip [1..] word)) 0

run :: (Eq q, Ord q, Ord s) => TM q s -> [s] -> Configuration q s
run tm word = fromJust $ getHaltingConf tm (getRunFromConf tm (initialConf tm word))

runSteps :: (Eq q, Ord q, Ord s) => TM q s -> [s] -> Int -> Maybe (Configuration q s)
runSteps tm word steps = getHaltingConf tm (take steps $ getRunFromConf tm (initialConf tm word))
