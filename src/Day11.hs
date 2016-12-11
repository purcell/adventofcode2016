module Day11 where

-- ( day11
-- )
import Day
import Data.List (subsequences)
import Data.Tree (Tree)
import qualified Data.Tree as T
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad

data Element
  = Polonium
  | Thulium
  | Promethium
  | Ruthenium
  | Cobalt
  deriving (Eq, Ord, Show)

data Item
  = Generator Element
  | Chip Element
  deriving (Eq, Ord, Show)

safeTogether :: Set Item -> Bool
safeTogether items =
  all
    safeChip
    [ n
    | (Chip n) <- S.elems items ]
  where
    safeChip n =
      Generator n `S.member` items ||
      null
        [ n'
        | (Generator n') <- S.elems items
        , n' /= n ]

data State = State
  { sLevel :: Int
  , sFloors :: [Set Item]
  } deriving (Eq, Ord, Show)

initialFloors =
  S.fromList <$>
  [ [ Generator Polonium
    , Generator Thulium
    , Chip Thulium
    , Generator Promethium
    , Generator Ruthenium
    , Chip Ruthenium
    , Generator Cobalt
    , Chip Cobalt
    ]
  , [Chip Polonium, Chip Promethium]
  , []
  , []
  ]

initialState = State 0 initialFloors

unfoldFloors :: Tree [State]
unfoldFloors = T.unfoldTree unfolder (S.singleton initialState, [initialState])
  where
    unfolder (seen, states) =
      ( states
      , [ (newSeen, next : states)
        | next <- nexts ])
      where
        nexts = filter (not . (`S.member` seen)) $ nextStates (head states)
        newSeen = S.union seen (S.fromList nexts)

nextStates :: State -> [State]
nextStates (State level floors) = do
  newLevel <- [level + 1, level - 1]
  guard $ newLevel >= 0 && newLevel < length floors
  guard $ newLevel > level || not (S.null (floors !! newLevel))
  load <- safeLoads (floors !! level) -- TODO: only choose one gen/chip pair
  case moveIfSafe floors load level newLevel of
    Just newFloors -> return $ State newLevel newFloors
    Nothing -> mempty

moveIfSafe :: [Set Item] -> Set Item -> Int -> Int -> Maybe [Set Item]
moveIfSafe floors load level newLevel = do
  let oldLevelItems = (floors !! level) `S.difference` load
      newLevelItems = (floors !! newLevel) `S.union` load
  guard $ safeTogether oldLevelItems
  guard $ safeTogether newLevelItems
  return
    [ if n == level
       then oldLevelItems
       else if n == newLevel
              then newLevelItems
              else fl
    | (n, fl) <- zip [0 ..] floors ]

safeLoads :: Set Item -> [Set Item]
safeLoads fl =
  [ group
  | group <-
     drop 1 $
     fmap S.fromList $ takeWhile ((<= 2) . length) $ subsequences $ S.elems fl
  , safeTogether group ]

partA _ =
  steps $ head $ dropWhile (not . complete . head) $ concat $ T.levels $ unfoldFloors
  where
    steps states = length states - 1
    complete (State 3 floors) =
      null
        [ True
        | (Chip _) <- concatMap S.elems $ take 3 floors ]
    complete _ = False

day11 = Day 11 (many anyChar) (return . show . partA) (return . const "TODO")
