module Day1
  ( day1
  , runInstructions)
where

import           Text.Parsec        hiding (State)
import           Text.Parsec.String


data TurnDirection = L | R

data Instruction = Instruction TurnDirection Integer


data Direction = N | E | S | W

type Blocks = Integer

data Position = Position { pFacing :: Direction
                         , pNetN   :: Blocks
                         , pNetE   :: Blocks }

shortestDistance :: Position -> Blocks
shortestDistance (Position _ n e) = abs n + abs e

initialPosition = Position N 0 0


forward :: Position -> Blocks -> Position
forward p@(Position N n e) b = p { pNetN = n + b }
forward p@(Position S n e) b = p { pNetN = n - b }
forward p@(Position E n e) b = p { pNetE = e + b }
forward p@(Position W n e) b = p { pNetE = e - b }

rotate :: Position -> TurnDirection -> Position
rotate p td = p { pFacing = rotated (pFacing p) td }
  where
    rotated N L = W
    rotated E L = N
    rotated S L = E
    rotated W L = S
    rotated d R = rotated (rotated (rotated d L) L) L

move :: Position -> Instruction -> Position
move p (Instruction td dist) = forward (rotate p td) dist



parseInstruction :: Parser Instruction
parseInstruction = Instruction <$> parseDirection <*> parseNumber
  where
    parseDirection = try (string "R" *> return R) <|> (string "L" *> return L)
    parseNumber = read <$> many1 digit

parseFile :: String -> IO (Either ParseError [Instruction])
parseFile = parseFromFile (sepBy parseInstruction (string ", ") <* newline <* eof)

runInstructions :: [Instruction] -> Blocks
runInstructions instrs = shortestDistance $ foldl move initialPosition instrs

day1 :: IO ()
day1 = do
  result <- parseFile "input/1.txt"
  case result of
    Right instrs -> print $ runInstructions instrs
    Left e -> error (show e)
