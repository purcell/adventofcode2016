{-# LANGUAGE RecordWildCards #-}

module Main where

import AdventOfCode
import Control.Monad.State
import Data.Char (chr)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybeToList)
import qualified Data.Vector as V
import Data.Vector (Vector)

newtype Reg =
  Reg Char
  deriving (Eq, Show)

data Value
  = LitVal !Int
  | RegVal !Reg
  deriving (Eq, Show)

data Instr
  = Copy !Value
         !Reg
  | JumpNotZero !Value
                !Value
  | Inc !Reg
  | Dec !Reg
  | Toggle !Value
  | Mult !Value
         !Value
         !Reg
  | NoOp
  | Out !Value
  deriving (Eq, Show)

parseInstr :: Parser Instr
parseInstr =
  (Dec <$> (string "dec " *> reg)) <|> (Inc <$> (string "inc " *> reg)) <|>
  (JumpNotZero <$> (string "jnz " *> val) <*> (string " " *> val)) <|>
  (Toggle <$> (string "tgl " *> val)) <|>
  (Copy <$> (string "cpy " *> val) <*> (string " " *> reg)) <|>
  (Out <$> (string "out " *> val))
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
  , stInstrs :: Vector Instr
  }

instance Show SimState where
  show (SimState pos regs instrs) =
    unlines
      (show regs : toList (V.map showins (V.zip (V.fromList [1 ..]) instrs)))
    where
      showins (n, i) =
        show n ++
        (if n == pos
           then ">> "
           else "  ") ++
        show i

type Sim = (State SimState)

run :: Sim [Int]
run = do
  (ran, output) <- runNext
  let produced = maybeToList output
  if ran
    then (produced ++) <$> run
    else return produced

runNext :: Sim (Bool, Maybe Int)
runNext = do
  SimState {..} <- get
  case stInstrs V.!? stPos of
    Just instr -> do
      (step, output) <- runInstr instr
      modify (\st -> st {stPos = stPos + step})
      return (True, output)
    _ -> return (False, Nothing)

modifyReg :: Reg -> (Int -> Int) -> Sim ()
modifyReg r@(Reg n) f = do
  prev <- getReg r
  modify
    (\st ->
       let regs = stRegs st
       in st {stRegs = M.insert n (f prev) regs})

getReg :: Reg -> Sim Int
getReg (Reg n) = (fromMaybe 0 . M.lookup n) <$> gets stRegs

getVal :: Value -> Sim Int
getVal (LitVal v) = return v
getVal (RegVal r) = getReg r

runMult :: Sim Int
runMult = do
  void $ runInstr (Mult (RegVal (Reg 'b')) (RegVal (Reg 'd')) (Reg 'a'))
  void $ runInstr (Copy (LitVal 0) (Reg 'c'))
  void $ runInstr (Copy (LitVal 0) (Reg 'd'))
  return 6

multPattern :: Vector Instr
multPattern =
  V.fromList
    [ Copy (RegVal (Reg 'b')) (Reg 'c')
    , Inc (Reg 'a')
    , Dec (Reg 'c')
    , JumpNotZero (RegVal (Reg 'c')) (LitVal (-2))
    , Dec (Reg 'd')
    , JumpNotZero (RegVal (Reg 'd')) (LitVal (-5))
    ]

runInstr :: Instr -> Sim (Int, Maybe Int)
runInstr (Copy val reg) =
  (getVal val >>= modifyReg reg . const) >> return (1, Nothing)
runInstr (JumpNotZero val1 val2) = do
  v <- getVal val1
  v2 <- getVal val2
  return $
    ( if v /= 0
        then v2
        else 1
    , Nothing)
runInstr (Inc reg) = modifyReg reg (+ 1) >> return (1, Nothing)
runInstr (Dec reg) = modifyReg reg (\i -> i - 1) >> return (1, Nothing)
runInstr (Toggle val) = do
  offset <- getVal val
  pos <- gets stPos
  toggleInstrAt (pos + offset)
  return (1, Nothing)
runInstr NoOp = return (1, Nothing)
runInstr (Mult val1 val2 reg) = do
  v1 <- getVal val1
  v2 <- getVal val2
  modifyReg reg (const (v1 * v2))
  return (1, Nothing)
runInstr (Out val) = do
  v <- getVal val
  return (1, Just v)

toggleInstrAt :: Int -> Sim ()
toggleInstrAt pos =
  modify
    (\s ->
       let prev = stInstrs s
       in s {stInstrs = prev V.// [(pos, toggleInstr (prev V.! pos))]})

toggleInstr :: Instr -> Instr
toggleInstr NoOp = error "toggling no-op"
toggleInstr (Copy val reg) = JumpNotZero val (RegVal reg)
toggleInstr (JumpNotZero val1 (RegVal val2)) = Copy val1 val2
toggleInstr (Inc reg) = Dec reg
toggleInstr (Dec reg) = Inc reg
toggleInstr (Toggle (RegVal reg)) = Inc reg
toggleInstr _ = NoOp

runSim :: Map Char Int -> [Instr] -> [Int]
runSim regs instrs = evalState run (SimState 0 regs (V.fromList instrs))

partA :: [Instr] -> String
partA instrs = chr <$> runSim M.empty instrs

main :: IO ()
main = runDay day25

day25 :: Day [Instr]
day25 =
  Day
    25
    (many (parseInstr <* newline))
    (return . show . partA)
    (return . show . const "TODO")
