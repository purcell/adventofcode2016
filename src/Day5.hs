module Day5
  ( day5
  )

where

import Data.Digest.Pure.MD5 as MD5
import Data.String


hashes :: String -> [String]
hashes seed = hexMD5 . (seed ++) . show <$> [0..]


hexMD5 = show . md5 . fromString


interesting :: String -> Bool
interesting s = "00000" == take 5 s


solution seed = take 8 $ (!! 5) <$> filter interesting (hashes seed)


day5 :: IO ()
day5 = putStrLn $ solution "ojvtpuvg"
