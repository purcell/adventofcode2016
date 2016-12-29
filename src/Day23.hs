module Main where

import AdventOfCode
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Sq

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
  deriving (Eq, Show)

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
  , stInstrs :: Seq Instr
  }

instance Show SimState where
  show (SimState pos regs instrs) =
    unlines (show regs : toList (Sq.mapWithIndex showins instrs))
    where
      showins n i =
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
  pos <- gets stPos
  nextInstrs <- Sq.take 6 . Sq.drop pos <$> gets stInstrs
  if Sq.null nextInstrs
    then return False
    else do
      step <-
        if nextInstrs == multPattern
          then runMult
          else runInstr (nextInstrs `Sq.index` 0)
      modify
        (\st ->
            st
            { stPos = stPos st + step
            })
      return True

modifyReg :: Reg -> (Int -> Int) -> Sim ()
modifyReg r@(Reg n) f = do
  prev <- getReg r
  modify
    (\st ->
        let regs = stRegs st
        in st
           { stRegs = M.insert n (f prev) regs
           })

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

multPattern :: Seq Instr
multPattern =
  Sq.fromList
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

toggleInstrAt :: Int -> Sim ()
toggleInstrAt pos =
  modify
    (\s ->
        s
        { stInstrs = Sq.adjust toggleInstr pos (stInstrs s)
        })

toggleInstr :: Instr -> Instr
toggleInstr NoOp = error "toggling no-op"
toggleInstr (Copy val reg) = JumpNotZero val (RegVal reg)
toggleInstr (JumpNotZero val1 (RegVal val2)) = Copy val1 val2
toggleInstr (Inc reg) = Dec reg
toggleInstr (Dec reg) = Inc reg
toggleInstr (Toggle (RegVal reg)) = Inc reg
toggleInstr _ = NoOp

finalA :: Map Char Int -> [Instr] -> Int
finalA regs instrs =
  (M.! 'a') $ stRegs $ execState run (SimState 0 regs (Sq.fromList instrs))

partA :: [Instr] -> Int
partA = finalA (M.singleton 'a' 7)

partB :: [Instr] -> Int
partB = finalA (M.singleton 'a' 12)

main :: IO ()
main = runDay day23

day23 :: Day [Instr]
day23 =
  Day
    23
    (many (parseInstr <* newline))
    (return . show . partA)
    (return . show . partB)
