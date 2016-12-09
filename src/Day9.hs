module Day9
  ( day9
  ) where

import Text.Parsec hiding (State)
import Text.Parsec.String

data Block
  = Repeat Int
           String
           [Block]
  | Plain String
  deriving (Show)

blockLenA :: Block -> Int
blockLenA (Repeat n s _) = n * length s
blockLenA (Plain s) = length s

blockLenB :: Block -> Int
blockLenB (Repeat n _ bs) = n * sum (blockLenB <$> bs)
blockLenB (Plain s) = length s

parseBlock :: Parser Block
parseBlock = try parseRepeat <|> (Plain <$> many1 letter)
  where
    parseRepeat = do
      len <- char '(' *> number <* char 'x'
      mult <- number <* char ')'
      rest <- count len anyChar
      case runP (many parseBlock <* eof) () "block" rest of
        Right subBlocks -> pure $ Repeat mult rest subBlocks
        Left err -> fail (show err)
    number = read <$> many1 digit

loadInput :: IO [Block]
loadInput = do
  result <- parseFromFile (many1 parseBlock <* newline <* eof) "input/9.txt"
  case result of
    Right xs -> return xs
    Left e -> error (show e)

day9 :: IO ()
day9 = do
  input <- loadInput
  print $ sum (blockLenA <$> input)
  print $ sum (blockLenB <$> input)
