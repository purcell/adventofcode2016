module Day
  ( loadDay
  , runDay
  , Day(..)
  , module P
  ) where

import Text.Parsec as P hiding (State)
import Text.Parsec.String as P
import System.Environment (getArgs)
import Control.Monad (when)

data Day i = Day
  { dayNum :: Int
  , dayParser :: Parser i
  , dayPartA :: i -> IO String
  , dayPartB :: i -> IO String
  }

loadDay :: Day i -> IO i
loadDay d = do
  result <-
    parseFromFile (dayParser d <* eof) ("input/" ++ show (dayNum d) ++ ".txt")
  case result of
    Right input -> return input
    Left e -> error (show e)

runDay :: Day i -> IO ()
runDay d = do
  args <- getArgs
  input <- loadDay d
  when (null args || "a" `elem` args) $
    do putStrLn $ banner "a"
       putStrLn =<< dayPartA d input
  when (null args || "b" `elem` args) $
    do putStrLn $ "\n" ++ banner "b"
       putStrLn =<< dayPartB d input
  where
    banner ab = "==== DAY " ++ show (dayNum d) ++ ab ++ " ====\n"
