module Day6
  ( day6
  ) where

import Text.Parsec hiding (State)
import Text.Parsec.String
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Map (Map)
import qualified Data.Map as M

frequencies :: [String] -> [Map Char Int]
frequencies =
  M.elems .
  foldl (M.unionWith (M.unionWith (+))) M.empty .
  fmap (M.fromList . zip [(0 :: Int) ..] . frequenciesInWord)

frequenciesInWord :: String -> [Map Char Int]
frequenciesInWord s = uncurry M.singleton <$> zip s (repeat 1)

foo :: [String] -> String
foo = map mostFrequent . frequencies
  where
    mostFrequent :: Map Char Int -> Char
    mostFrequent = fst . maximumBy (comparing snd) . M.toList

loadInput :: IO [String]
loadInput = do
  result <- parseFromFile (many1 (many1 letter <* newline) <* eof) "input/6.txt"
  case result of
    Right xs -> return xs
    Left e -> error (show e)

day6 :: IO ()
day6 = do
  strings <- loadInput
  putStrLn $ foo strings
