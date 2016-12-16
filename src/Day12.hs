module Main where

import Day
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M

data Reg =
  Reg Char
  deriving (Show)

data Value
  = LitVal Int
  | RegVal Reg
  deriving (Show)

data Instr
  = Copy Value
         Reg
  | JumpNotZero Value
                Int
  | Inc Reg
  | Dec Reg
  deriving (Show)

parseInstr :: Parser Instr
parseInstr =
  (Dec <$> (string "dec " *> reg)) <|> (Inc <$> (string "inc " *> reg)) <|>
  (JumpNotZero <$> (string "jnz " *> val) <*> (string " " *> step)) <|>
  (Copy <$> (string "cpy " *> val) <*> (string " " *> reg))
  where
    reg = Reg <$> letter
    val = (RegVal <$> reg) <|> (LitVal <$> num)
    step = num
    num = do
      sign <- option '+' (char '-')
      digits <- many1 digit
      return $
        (if sign == '-'
           then -1
           else 1) *
        read digits

data SimState = SimState
  { stPos :: Int
  , stRegs :: (Map Char Int)
  }

type Sim = ReaderT [Instr] (State SimState)

run :: Sim ()
run = do
  pos <- gets stPos
  instrs <- ask
  when (pos >= 0 && pos < length instrs) $
    do step <- lift $ runInstr (instrs !! pos)
       modify
         (\st ->
             st
             { stPos = stPos st + step
             })
       run

modifyReg :: Reg -> (Int -> Int) -> State SimState ()
modifyReg r@(Reg n) f = do
  prev <- getReg r
  modify
    (\st ->
        let regs = stRegs st
        in st
           { stRegs = M.insert n (f prev) regs
           })

getReg :: Reg -> State SimState Int
getReg (Reg n) = (fromMaybe 0 . M.lookup n) <$> gets stRegs

getVal :: Value -> State SimState Int
getVal (LitVal v) = return v
getVal (RegVal r) = getReg r

runInstr :: Instr -> State SimState Int
runInstr (Copy val reg) = (getVal val >>= modifyReg reg . const) >> return 1
runInstr (JumpNotZero val step) = do
  v <- getVal val
  return $
    if v /= 0
      then step
      else 1
runInstr (Inc reg) = modifyReg reg (+ 1) >> return 1
runInstr (Dec reg) = modifyReg reg (\i -> i - 1) >> return 1

finalA :: Map Char Int -> [Instr] -> Int
finalA regs =
  (M.! 'a') . stRegs . flip execState (SimState 0 regs) . runReaderT run

partA = finalA M.empty

partB = finalA (M.singleton 'c' 1)

main =
  runDay $
  Day
    12
    (many (parseInstr <* newline))
    (return . show . partA)
    (return . show . partB)
