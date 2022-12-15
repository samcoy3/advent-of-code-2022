module Days.Day15 (runDay) where

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
import Util.Pair (Pair(..))
import Data.Ord (comparing)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
data SensorInfo = SensorInfo
  { sensor :: Pair Int,
    nearestBeacon :: Pair Int,
    distanceFromBeacon :: Int
  }
  deriving (Show)

type Input = [SensorInfo]

type OutputA = Int

type OutputB = Int

------------ PARSER ------------
inputParser :: Parser Input
inputParser = sortOn (fst . getPair . sensor) <$> sensorInfo `sepBy` endOfLine
  where
    point = do
      "x="
      x <- signed decimal
      ", y="
      y <- signed decimal
      return $ Pair (x, y)
    sensorInfo = do
      "Sensor at "
      sensor <- point
      ": closest beacon is at "
      nearestBeacon <- point
      return SensorInfo {distanceFromBeacon = distance sensor nearestBeacon, ..}

------------ PART A ------------
distance :: Pair Int -> Pair Int -> Int
distance a b = sum . fmap abs $ (-) <$> a <*> b

getExclusionZoneAtYCoord :: Int -> SensorInfo -> Maybe (Int, Int)
getExclusionZoneAtYCoord y SensorInfo {..} =
  let distanceFromYCoord = abs $ y - (snd . getPair $ sensor)
      sensorX = fst . getPair $ sensor
   in if distanceFromYCoord > distanceFromBeacon
        then Nothing
        else
          Just
            ( sensorX - (distanceFromBeacon - distanceFromYCoord),
              sensorX + (distanceFromBeacon - distanceFromYCoord)
            )

combineIntervals :: [(Int, Int)] -> [(Int, Int)]
combineIntervals = foldl' combineIntervals' [] . sortOn fst
  where
    combineIntervals' [] interval = [interval]
    combineIntervals' intervals@((b, e) : _) (b', e') =
      if b' <= e
        then (b, max e e') : tail intervals
        else (b', e') : intervals

partA :: Input -> OutputA
partA sensorInfos =
  (+ (-numberOfBeaconsInZone))
    . sum
    . fmap (\(a, b) -> b - a + 1)
    . combineIntervals
    . mapMaybe (getExclusionZoneAtYCoord yCoord)
    $ sensorInfos
  where
    yCoord = 2_000_000
    numberOfBeaconsInZone = length . filter ((== yCoord) . snd) . nub . fmap (getPair . nearestBeacon) $ sensorInfos

------------ PART B ------------
partB :: Input -> Int
partB sensorInfos =
  (\((x, _) : _, y) -> ((x - 1) * 4_000_000) + y)
    . head
    . filter ((/= 1) . length . fst)
    $ getIntervalsAtCoord
      <$> [0 .. 4_000_000]
  where
    getIntervalsAtCoord y = (,y) . combineIntervals . mapMaybe (getExclusionZoneAtYCoord y) $ sensorInfos
