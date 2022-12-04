module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( sepBy, decimal, char, endOfLine, Parser )
import Util.Parsers (around)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (interval `around` char ',') `sepBy` endOfLine
  where
    interval = decimal `around` char '-'

------------ TYPES ------------
type Interval = (Int, Int)

type Input = [(Interval, Interval)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Predicate for wholly-contained intervals
contains :: Interval -> Interval -> Bool
contains (a, b) (c, d) = a <= c && b >= d

partA :: Input -> OutputA
partA = length . filter (\(i1, i2) -> (i1 `contains` i2) || (i2 `contains` i1))

------------ PART B ------------
-- Predicate for overlapping intervals
-- (checks whether the start of one interval is contained within the other)
-- (nned to check both directions to be sure of overlap)
containsStartOf :: Interval -> Interval -> Bool
containsStartOf (a, b) (c, _) = a <= c && c <= b

partB :: Input -> OutputB
partB = length . filter (\(i1, i2) -> (i1 `containsStartOf` i2) || (i2 `containsStartOf` i1))
