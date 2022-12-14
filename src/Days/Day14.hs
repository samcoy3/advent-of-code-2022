module Days.Day14 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
import Util.Parsers (around)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Map.fromList . fmap (,Rock) <$> rockPositions
  where
    rockPositions = concatMap processSegments <$> segments `sepBy` endOfLine
    processSegments segments =
      concat $
        zipWith
          ( \(x1, y1) (x2, y2) ->
              [(x, y) | x <- x1 `U.to` x2, y <- y1 `U.to` y2]
          )
          segments
          (tail segments)
    segments = decimal `around` char ',' `sepBy` string " -> "

------------ TYPES ------------
data Object = Rock | Sand
  deriving (Show, Eq)

type CaveSystem = Map (Int, Int) Object

data Part = PartA | PartB
  deriving (Eq)

type Input = CaveSystem

type OutputA = Int

type OutputB = Int

------------ PART A ------------
addSand :: Int -> Part -> CaveSystem -> [(Int, Int)] -> Maybe [(Int, Int)]
addSand yMax part caves stack =
  let addSand' [] = Nothing
      addSand' stack@((x, y) : _) =
        if
            | y == yMax && part == PartA -> Nothing
            | y == yMax + 1 && part == PartB -> Just stack
            | (x, y + 1) `Map.notMember` caves -> addSand' $ (x, y + 1) : stack
            | (x - 1, y + 1) `Map.notMember` caves -> addSand' $ (x - 1, y + 1) : stack
            | (x + 1, y + 1) `Map.notMember` caves -> addSand' $ (x + 1, y + 1) : stack
            | otherwise -> Just stack
   in addSand' stack

addSandUntilFallsOff :: CaveSystem -> [(Int, Int)]
addSandUntilFallsOff caves =
  let (_, _, _, yMax) = U.mapBoundingBox caves
   in unfoldr
        ( \c -> case uncurry (addSand yMax PartA) c of
            Just stack -> Just (head stack, (Map.insert (head stack) Sand (fst c), tail stack))
            Nothing -> Nothing
        )
        (caves, [(500, 0)])

partA :: Input -> OutputA
partA = length . addSandUntilFallsOff

------------ PART B ------------
addSandUntilBlocksSource :: CaveSystem -> Int
addSandUntilBlocksSource caves =
  let (_, _, _, yMax) = U.mapBoundingBox caves
   in fromJust
        . elemIndex (500, 0)
        . unfoldr
          (\c -> uncurry (addSand yMax PartB) c >>= (\stack -> Just (head stack, (Map.insert (head stack) Sand (fst c), tail stack))))
        $ (caves, [(500, 0)])

partB :: Input -> Int
partB = (+ 1) . addSandUntilBlocksSource
