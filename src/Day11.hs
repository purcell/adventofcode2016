module Main where

import AdventOfCode
import qualified Data.Set as S
import Data.List (intercalate, find, sort, tails, groupBy)
import Data.Monoid ((<>))
import Data.Function (on)

data Building = Building
  { bLevel :: !Int
  , bItems :: ![(Item, Int)]
  } deriving (Eq, Ord)

data Direction
  = Up
  | Down
  deriving (Eq, Ord, Show)

data ItemType
  = Generator
  | Microchip
  deriving (Eq, Ord)

data Element
  = Thulium
  | Polonium
  | Promethium
  | Ruthenium
  | Cobalt
  | Hydrogen
  | Lithium
  | Elerium
  | Dilithium
  deriving (Eq, Ord, Show)

data Item = Item
  { iType :: !ItemType
  , iElement :: !Element
  } deriving (Eq)

instance Ord Item where
  compare (Item t1 e1) (Item t2 e2) = compare (e1, t1) (e2, t2)

data Step =
  Step !Direction
       ![Item]
  deriving (Show)

instance Show Building where
  show (Building level items) = unlines (showlevel <$> reverse [0 .. 3])
    where
      showlevel n =
        show (n + 1) ++
        (if n == level
           then " > "
           else "   ") ++
        intercalate
          "  "
          [ show i
          | (i, f) <- sort items
          , f == n ]

instance Show Item where
  show (Item Generator e) = "G-" ++ show e
  show (Item Microchip e) = "M-" ++ show e

fingerprint :: Building -> (Int, [[Int]])
fingerprint (Building level items) = (level, sort pairFloors)
  where
    pairFloors = fmap snd <$> groupBy ((==) `on` iElement . fst) (sort items)

bestFrom :: Building -> Maybe (Building, [Step])
bestFrom b = find (complete . fst) (bfsOn (fingerprint . fst) nexts (b, []))

complete :: Building -> Bool
complete (Building _ items) = all ((== 3) . snd) items

isSafe :: Building -> Bool
isSafe (Building _ floors) = all chipIsSafeOnFloor chips
  where
    chips =
      [ (el, fl)
      | (Item Microchip el, fl) <- floors ]
    chipIsSafeOnFloor (el, chipFloor) = null gensHere || el `elem` gensHere
      where
        gensHere =
          [ el'
          | (Item Generator el', f) <- floors
          , f == chipFloor ]

runStep :: Building -> Step -> Building
runStep (Building level floors) (Step dir items) = Building level' floors'
  where
    level' =
      if dir == Up
        then level + 1
        else level - 1
    floors' = maybeMove <$> floors
    maybeMove (i, f) =
      if i `elem` items
        then (i, level')
        else (i, f)

nexts :: (Building, [Step]) -> [(Building, [Step])]
nexts (bldg, steps) =
  [ (bldg', steps ++ [s])
  | s <- possibleSteps bldg
  , let bldg' = runStep bldg s
  , isSafe bldg' ]

possibleSteps :: Building -> [Step]
possibleSteps (Building level floors) =
  Step <$> validDirections <*> onesOrTwos currentItems
  where
    validDirections =
      (if level < 3
         then pure Up
         else mempty) <>
      (if level > 0
         then pure Down
         else mempty)
    currentItems =
      [ i
      | (i, n) <- floors
      , n == level ]

onesOrTwos :: [a] -> [[a]]
onesOrTwos xs =
  [ [x, y]
  | x:ys <- tails xs
  , y <- ys ] <>
  map pure xs

exampleBuilding :: Building
exampleBuilding =
  Building
    0
    [ (Item Microchip Hydrogen, 0)
    , (Item Microchip Lithium, 0)
    , (Item Generator Hydrogen, 1)
    , (Item Generator Lithium, 2)
    ]

buildingA :: Building
buildingA =
  Building
    0
    [ (Item Generator Polonium, 0)
    , (Item Generator Thulium, 0)
    , (Item Microchip Thulium, 0)
    , (Item Generator Promethium, 0)
    , (Item Generator Ruthenium, 0)
    , (Item Microchip Ruthenium, 0)
    , (Item Generator Cobalt, 0)
    , (Item Microchip Cobalt, 0)
    , (Item Microchip Polonium, 1)
    , (Item Microchip Promethium, 1)
    ]

buildingB :: Building
buildingB =
  buildingA
  { bItems =
    bItems buildingA ++
    [ (Item Generator Elerium, 0)
    , (Item Microchip Elerium, 0)
    , (Item Generator Dilithium, 0)
    , (Item Microchip Dilithium, 0)
    ]
  }

solve :: Building -> Maybe Int
solve = fmap (length . snd) . bestFrom

test :: Building -> IO ()
test b = putStrLn $ unlines $ concatMap (\(b', s) -> [b', "", s, ""]) stages
  where
    stages = zip ("" : numberedSteps) (show <$> scanl runStep b steps)
    Just (_, steps) = bestFrom b
    numberedSteps =
      [ show n ++ ": " ++ show s
      | (n, s) <- zip [(1 :: Int) ..] steps ]

main :: IO ()
main =
  runDay $
  Day
    11
    (many anyChar)
    (return . show . const (solve buildingA))
    (return . show . const (solve buildingB))
