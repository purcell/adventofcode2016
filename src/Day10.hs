module Day10
  ( day10
  ) where

import Day
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)

newtype BotNo = BotNo
  { getBotNo :: Int
  } deriving (Eq, Ord, Show)

data Receiver
  = BotReceiver BotNo
  | OutputReceiver Int

data Instr
  = Pass { bName :: BotNo
         , bLow :: Receiver
         , bHigh :: Receiver}
  | Input { iVal :: Int
          , iBot :: BotNo}

parseInstr :: Parser Instr
parseInstr =
  (Input <$> (string "value " *> number) <*> (string " goes to bot " *> botno)) <|>
  (Pass <$> (string "bot " *> botno) <*> (string " gives low to " *> receiver) <*>
   (string " and high to " *> receiver))
  where
    botno = BotNo <$> number
    number = read <$> many1 digit
    receiver =
      (BotReceiver <$> (string "bot " *> botno)) <|>
      (OutputReceiver <$> (string "output " *> number))

type BotState = State (Map BotNo [Int])

runInstrs :: [Instr] -> BotState ()
runInstrs [] = return ()
runInstrs ((Input v b):xs) = putVal b v >> runInstrs xs
runInstrs (p@(Pass b lo hi):xs) = do
  vals <- getVal b
  case sort vals of
    [v1, v2] -> do
      pass lo v1
      pass hi v2
      runInstrs xs
    _ -> runInstrs (xs ++ [p])

pass :: Receiver -> Int -> BotState ()
pass (BotReceiver b) v = putVal b v
pass (OutputReceiver b) v = return () -- TODO

getVal :: BotNo -> BotState [Int]
getVal b = (fromMaybe [] . M.lookup b) <$> get

putVal :: BotNo -> Int -> BotState ()
putVal b i = do
  vals <- getVal b
  modify (M.insert b (i : vals))

partA =
  filter (\(_, vs) -> sort vs == [17, 61]) .
  M.toList . flip execState M.empty . runInstrs

partB = return "TODO"

-- partB instrs = evalState mult M.empty
--   where
--     mult = product <$> mapM (output instrs) [0, 1, 2]
day10 =
  Day
    10
    (many1 (parseInstr <* newline))
    (return . show . partA)
    (return . show . partB)
