module Day10
  ( day10
  ) where

import Day
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Data.List (nub, sort)

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

inputsOf :: [Instr] -> BotNo -> BotState [Int]
inputsOf instrs botno = do
  cached <- M.lookup botno <$> get
  case cached of
    Just vs -> return vs
    _ -> do
      mins <- mapM (fmap minimum . inputsOf instrs) minfeeds
      maxes <- mapM (fmap maximum . inputsOf instrs) maxfeeds
      let inputs = values ++ mins ++ maxes
      modify (M.insert botno inputs)
      return inputs
  where
    values =
      [ v
      | (Input v b) <- instrs
      , b == botno ]
    minfeeds =
      [ b
      | (Pass b (BotReceiver lo) _) <- instrs
      , lo == botno ]
    maxfeeds =
      [ b
      | (Pass b _ (BotReceiver hi)) <- instrs
      , hi == botno ]

runInstrs :: [Instr] -> [(BotNo, [Int])]
runInstrs instrs = M.toList $ execState (mapM_ (inputsOf instrs) knownBots) M.empty
  where
    knownBots =
      nub $
      [ b
      | (Input v b) <- instrs ] ++
      concat
        [ [b, lo]
        | (Pass b (BotReceiver lo) _) <- instrs ] ++
      concat
        [ [b, hi]
        | (Pass b _ (BotReceiver hi)) <- instrs ]

day10 =
  Day
    10
    (many1 (parseInstr <* newline))
    (return . show . filter (\(b, vs) -> sort vs == [17, 61]) . runInstrs)
    (return . const "TODO")
