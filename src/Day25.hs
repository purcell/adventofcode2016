{-# LANGUAGE RecordWildCards #-}

module Main where

import AdventOfCode
import Control.Monad.State
import Data.Foldable (toList)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as Sq
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
  , stOutput :: Seq Int
  , stFinished :: SimState -> Bool
  }

instance Show SimState where
  show (SimState pos regs instrs output _) =
    unlines
      (show regs :
       show output : toList (V.map showins (V.zip (V.fromList [1 ..]) instrs)))
    where
      showins (n, i) =
        show n ++
        (if n == pos
           then ">> "
           else "  ") ++
        show i

type Sim = (State SimState)

run :: Sim ()
run = do
  ran <- runNext
  when ran run

runNext :: Sim Bool
runNext = do
  s@SimState {..} <- get
  if stFinished s
    then return False
    else do
      step <- runInstr (stInstrs V.! stPos)
      modify (\st -> st {stPos = stPos + step})
      return True

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

runInstr :: Instr -> Sim Int
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
runInstr NoOp = return 1
runInstr (Mult val1 val2 reg) = do
  v1 <- getVal val1
  v2 <- getVal val2
  modifyReg reg (const (v1 * v2))
  return 1
runInstr (Out val) = do
  v <- getVal val
  modify (\s -> s {stOutput = stOutput s Sq.|> v})
  return 1

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

runSim :: (SimState -> Bool) -> Map Char Int -> [Instr] -> SimState
runSim done regs instrs =
  execState run (SimState 0 regs (V.fromList instrs) Sq.empty done)

partA :: [Instr] -> Maybe Int
partA instrs = find ((target ==) . stOutput . runA) [0 ..]
  where
    runA a = runSim done (M.singleton 'a' a) instrs
    done s = Sq.length (stOutput s) > 20
      -- let output = stOutput s
      --     outlen = Sq.length output
      -- in (output /= Sq.take outlen target) || outlen >= Sq.length target
    target = Sq.fromList (take 100 (cycle [0, 1]))

main :: IO ()
main = runDay day25

day25 :: Day [Instr]
day25 =
  Day
    25
    (many (parseInstr <* newline))
    (return . show . partA)
    (return . show . const "TODO")
