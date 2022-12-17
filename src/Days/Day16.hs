module Days.Day16 (runDay) where

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
import Control.Applicative ((<|>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Map.fromList <$> valve `sepBy` endOfLine
  where
    valve = do
      "Valve "
      name <- many1 letter
      " has flow rate="
      flow <- decimal
      "; tunnels lead to valves " <|> "; tunnel leads to valve "
      adjacents <- many1 letter `sepBy` ", "
      return (name, ValveData flow (Set.fromList adjacents))

------------ TYPES ------------
type ValveName = String

data ValveData = ValveData {flow :: Int, adjacentValves :: Set ValveName}
  deriving (Show, Eq, Ord)

type Valves = Map ValveName ValveData

type ValveDistances = Map ValveName (Set (ValveName, Int))

type Input = Valves

type OutputA = Int

type OutputB = Void

------------ PART A ------------
bfs :: Valves -> ValveName -> Set (ValveName, Int)
bfs valves start = bfs' valves (Set.singleton start) (Set.singleton start) 1
  where
    bfs' valves explored frontier distance =
      let newFrontier = (Set.\\ explored) . Set.unions $ Set.map (adjacentValves . (valves Map.!)) frontier
       in if Set.null newFrontier
            then Set.empty
            else
              Set.union (Set.map (,distance) newFrontier) $
                bfs' valves (Set.union explored newFrontier) newFrontier (distance + 1)

computeDistances :: Valves -> ValveDistances
computeDistances valves =
  let importantValves = Map.keys $ Map.filterWithKey (\name v -> name == "AA" || flow v /= 0) valves
   in Map.fromList $
        (\k -> (k, Set.filter ((/= 0) . flow . (valves Map.!) . fst) $ bfs valves k)) <$> importantValves

findBestSequence :: ValveName -> Valves -> Int
findBestSequence start valves = findBestSequence' (Set.singleton start) start valves (computeDistances valves) 30
  where
    findBestSequence' openValves currentValve valves distances remainingMinutes =
      if remainingMinutes <= 0
        then 0
        else
          let possibleNextMoves =
                Set.filter ((`Set.notMember` openValves) . fst) $
                  distances Map.! currentValve
              accruedFlow = remainingMinutes * flow (valves Map.! currentValve)
           in (+ accruedFlow) . maximum $
                Set.map (\(next, dist) -> findBestSequence' (Set.insert next openValves) next valves distances (remainingMinutes - (dist + 1))) possibleNextMoves

partA :: Input -> OutputA
partA = findBestSequence "AA"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
