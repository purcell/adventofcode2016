module Main where

import AdventOfCode

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

main =
  runDay $
  Day
    9
    (many1 parseBlock <* newline)
    (return . show . sum . fmap blockLenA)
    (return . show . sum . fmap blockLenB)
