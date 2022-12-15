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
import Util.Pair (Pair(..), (<+>), (<->))
import Data.Ord (comparing)
import Data.Ratio ((%))
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
    numberOfBeaconsInZone =
      length
        . filter ((== yCoord) . snd)
        . nub
        . fmap (getPair . nearestBeacon)
        $ sensorInfos

------------ PART B ------------
sensorToDiamondBoundaries :: SensorInfo -> ([(Pair Int, Pair Int)], [(Pair Int, Pair Int)])
sensorToDiamondBoundaries SensorInfo {..} =
  let [top, left, bottom, right] =
        (<+> sensor) . Pair
          <$> [ (0, distanceFromBeacon + 1),
                (-(distanceFromBeacon + 1), 0),
                (0, -(distanceFromBeacon + 1)),
                (distanceFromBeacon + 1, 0)
              ]
   in ([(left, top), (bottom, right)], [(top, right), (left, bottom)])

overlaps :: (Pair Int, Pair Int) -> (Pair Int, Pair Int) -> Maybe (Pair Int, Pair Int)
overlaps l1@(p11, p12) l2@(p21, p22) =
  if
      | l1 == l2 -> Nothing
      | p12 == p21 || p11 == p22 || p11 == p21 || p12 == p22 -> Nothing
      | p12 `liesOn` l2 && p11 `liesOn` l2 -> Just l1
      | p12 `liesOn` l2 -> Just (p21, p12)
      | otherwise -> Nothing

liesOn :: Pair Int -> (Pair Int, Pair Int) -> Bool
liesOn p l@(q1, q2) =
  let dist a b = (\(Pair (x, y)) -> sqrt $ fromIntegral x ** 2 + fromIntegral y ** 2) $ a <-> b
   in dist p q1 + dist p q2 == dist q1 q2

intersects :: (Pair Int, Pair Int) -> (Pair Int, Pair Int) -> Maybe (Pair Int)
intersects l1@(p1, p2) l2@(p3, _) =
  if sum p1 `mod` 2 /= sum p3 `mod` 2
    then Nothing
    else
      let delta = (sum p3 - sum p1) `div` 2
          p = p1 <+> Pair (delta, delta)
       in if p `liesOn` l1 && p `liesOn` l2 then U.traceShowIdWithContext (l1, l2) $ Just p else Nothing

partB :: Input -> [Pair Int]
partB sensorInfos =
  let (forwardBoundaries', backwardBoundaries') = unzip $ sensorToDiamondBoundaries <$> sensorInfos
      forwardBoundaries = foldr1 (++) forwardBoundaries'
      backwardBoundaries = foldr1 (++) backwardBoundaries'
      overlappingForward = catMaybes $ overlaps <$> forwardBoundaries <*> forwardBoundaries
      overlappingBackward = catMaybes $ overlaps <$> backwardBoundaries <*> backwardBoundaries
      (Pair (x, y)) = head . catMaybes $ intersects <$> overlappingForward <*> overlappingBackward
   in -- (4_000_000 * x) + y
      catMaybes $ intersects <$> overlappingForward <*> overlappingBackward

--   (sensorToDiamondBoundaries (head sensorInfos), head sensorInfos)

partB' :: Input -> Int
partB' sensorInfos =
  (\((x, _) : _, y) -> ((x - 1) * 4_000_000) + y)
    . head
    . filter ((/= 1) . length . fst)
    $ getIntervalsAtCoord
      <$> [0 .. 4_000_000]
  where
    getIntervalsAtCoord y = (,y) . combineIntervals . mapMaybe (getExclusionZoneAtYCoord y) $ sensorInfos
