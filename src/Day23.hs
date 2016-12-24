module Main where

import Day
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Sq

data Reg =
  Reg !Char
  deriving (Show)

data Value
  = LitVal !Int
  | RegVal !Reg
  deriving (Show)

data Instr
  = Copy !Value
         !Reg
  | JumpNotZero !Value
                !Value
  | Inc !Reg
  | Dec !Reg
  | Toggle !Value
  deriving (Show)

parseInstr :: Parser Instr
parseInstr =
  (Dec <$> (string "dec " *> reg)) <|> (Inc <$> (string "inc " *> reg)) <|>
  (JumpNotZero <$> (string "jnz " *> val) <*> (string " " *> val)) <|>
  (Toggle <$> (string "tgl " *> val)) <|>
  (Copy <$> (string "cpy " *> val) <*> (string " " *> reg))
  where
    reg = Reg <$> letter
    val = (RegVal <$> reg) <|> (LitVal <$> num)
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
  , stRegs :: Map Char Int
  , stInstrs :: Seq (Maybe Instr)
  }

type Sim = (State SimState)

run :: Sim ()
run = do
  pos <- gets stPos
  instrs <- gets stInstrs
  when (pos >= 0 && pos < Sq.length instrs) $
    do step <-
         case instrs `Sq.index` pos of
           Just i -> runInstr i
           _ -> return 1
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
runInstr (JumpNotZero val1 val2) = do
  v <- getVal val1
  v2 <- getVal val2
  return $
    if v /= 0
      then v2
      else 1
runInstr (Inc reg) = modifyReg reg (+ 1) >> return 1
runInstr (Dec reg) = modifyReg reg (\i -> i - 1) >> return 1
runInstr (Toggle val) = do
  offset <- getVal val
  pos <- gets stPos
  toggleInstrAt (pos + offset)
  return 1

toggleInstrAt :: Int -> State SimState ()
toggleInstrAt pos =
  modify
    (\s ->
        s
        { stInstrs = Sq.adjust toggleInstr pos (stInstrs s)
        })

toggleInstr :: Maybe Instr -> Maybe Instr
toggleInstr Nothing = error "untoggling an invalid instr"
toggleInstr (Just (Copy val reg)) = Just (JumpNotZero val (RegVal reg))
toggleInstr (Just (JumpNotZero val1 (RegVal val2))) = Just (Copy val1 val2)
toggleInstr (Just (Inc reg)) = Just (Dec reg)
toggleInstr (Just (Dec reg)) = Just (Inc reg)
toggleInstr (Just (Toggle (RegVal reg))) = Just (Inc reg)
toggleInstr _ = Nothing

finalA :: Map Char Int -> [Instr] -> Int
finalA regs instrs =
  (M.! 'a') $
  stRegs $ execState run (SimState 0 regs (Just <$> Sq.fromList instrs))

partA = finalA (M.singleton 'a' 7)

partB = finalA (M.singleton 'a' 12)

main =
  runDay $
  Day
    23
    (many (parseInstr <* newline))
    (return . show . partA)
    (return . show . partB)
