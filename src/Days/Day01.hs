module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List ( sortOn )

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( count, sepBy, decimal, endOfLine, Parser )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (decimal `sepBy` endOfLine) `sepBy` count 2 endOfLine

------------ TYPES ------------
type Input = [[Int]]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = maximum . fmap sum

------------ PART B ------------
partB :: Input -> OutputB
partB = sum . take 3 . sortOn negate . fmap sum
