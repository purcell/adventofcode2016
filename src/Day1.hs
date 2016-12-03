{-# LANGUAGE TupleSections #-}
module Day1
  ( day1
  )
where

import           Data.List          (groupBy)
import           Data.Maybe         (listToMaybe)
import           Data.Set           (Set)
import qualified Data.Set           as S
import           Text.Parsec        hiding (State)
import           Text.Parsec.String


data TurnDirection = L | R

data Instruction = Instruction { iDir    :: TurnDirection
                               , iBlocks :: Blocks}

data Direction = N | E | S | W
  deriving Show


type Blocks = Int

data Position = Position { pNetN :: Blocks
                         , pNetE :: Blocks }
                deriving (Eq, Ord, Show)

shortestDistance :: Position -> Blocks
shortestDistance (Position n e) = abs n + abs e

move :: Position -> Direction -> Position
move p@(Position n e) N = p { pNetN = n + 1 }
move p@(Position n e) S = p { pNetN = n - 1 }
move p@(Position n e) E = p { pNetE = e + 1 }
move p@(Position n e) W = p { pNetE = e - 1 }

rotate :: Direction -> TurnDirection -> Direction
rotate N L = W
rotate E L = N
rotate S L = E
rotate W L = S
rotate d R = rotate (rotate (rotate d L) L) L


parseInstruction :: Parser Instruction
parseInstruction = Instruction <$> parseDirection <*> parseNumber
  where
    parseDirection = try (string "R" *> return R) <|> (string "L" *> return L)
    parseNumber = read <$> many1 digit

parseFile :: String -> IO (Either ParseError [Instruction])
parseFile = parseFromFile (sepBy parseInstruction (string ", ") <* newline <* eof)

runInstructions :: [Instruction] -> [Position]
runInstructions instrs = scanl move (Position 0 0) moves
  where
    moves = concatMap expand (zip directions distances)
    expand (dir, dist) = replicate dist dir
    directions = tail $ scanl rotate N (iDir <$> instrs)
    distances = iBlocks <$> instrs

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = go S.empty
  where go _ [] = Nothing
        go seen (x:xs) = if x `S.member` seen then Just x
                         else go (S.insert x seen) xs

loadInput :: IO [Instruction]
loadInput = do
  result <- parseFile "input/1.txt"
  case result of
    Right instrs -> return instrs
    Left e -> error (show e)

day1 :: IO ()
day1 = do
  instrs <- loadInput
  let positions = runInstructions instrs
  print $ shortestDistance $ last positions
  print $ shortestDistance <$> firstDuplicate positions
