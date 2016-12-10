module Day10
  ( day10
  ) where

import Day
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Writer
import Data.List (sort)
import Data.Maybe (fromMaybe)

newtype BotNo =
  BotNo Int
  deriving (Eq, Ord, Show)

newtype OutputNo = OutputNo
  { getOutputNo :: Int
  } deriving (Eq, Ord, Show)

data Receiver
  = BotReceiver BotNo
  | OutputReceiver OutputNo

data Instr
  = PassLowHigh BotNo
                Receiver
                Receiver
  | Input Int
          BotNo

parseInstr :: Parser Instr
parseInstr =
  (Input <$> (string "value " *> number) <*> (string " goes to bot " *> botno)) <|>
  (PassLowHigh <$> (string "bot " *> botno) <*>
   (string " gives low to " *> receiver) <*>
   (string " and high to " *> receiver))
  where
    botno = BotNo <$> number
    number = read <$> many1 digit
    receiver =
      (BotReceiver <$> (string "bot " *> botno)) <|>
      (OutputReceiver . OutputNo <$> (string "output " *> number))

type BotState = WriterT [(OutputNo, Int)] (State (Map BotNo [Int]))

runInstrs :: [Instr] -> BotState ()
runInstrs [] = return ()
runInstrs (Input v b:xs) = putVal b v >> runInstrs xs
runInstrs (p@(PassLowHigh b lo hi):xs) = do
  vals <- getVal b
  case sort vals of
    [v1, v2] -> do
      passVal lo v1
      passVal hi v2
      runInstrs xs
    _ -> runInstrs (xs ++ [p])

passVal :: Receiver -> Int -> BotState ()
passVal (BotReceiver b) v = putVal b v
passVal (OutputReceiver b) v = tell [(b, v)]

getVal :: BotNo -> BotState [Int]
getVal b = (fromMaybe [] . M.lookup b) <$> get

putVal :: BotNo -> Int -> BotState ()
putVal b i = do
  vals <- getVal b
  modify (M.insert b (i : vals))

partA :: [Instr] -> [(BotNo, [Int])]
partA =
  filter (\(_, vs) -> sort vs == [17, 61]) .
  M.toList . flip execState M.empty . runWriterT . runInstrs

partB :: [Instr] -> Int
partB instrs =
  product $ map snd $ filter ((`elem` [0, 1, 2]) . getOutputNo . fst) outputs
  where
    outputs = flip evalState M.empty $ execWriterT (runInstrs instrs)

day10 :: Day [Instr]
day10 =
  Day
    10
    (many1 (parseInstr <* newline))
    (return . show . partA)
    (return . show . partB)
