module Day9
  ( day9
  ) where

import Text.Parsec hiding (State)
import Text.Parsec.String

data Block
  = Repeat Int
           String
  | Plain String

expandBlock :: Block -> String
expandBlock (Repeat n s) = concat $ replicate n s
expandBlock (Plain s) = s

parseBlock :: Parser Block
parseBlock = (try parseRepeat) <|> (Plain <$> many1 letter)
  where
    parseRepeat = do
      len <- char '(' *> number <* char 'x'
      mult <- number <* char ')'
      rest <- count len anyChar
      pure $ Repeat mult rest
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
  let result = sum (length . expandBlock <$> input)
  print result
