module Days.Day06 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List ( nub )

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text ( Parser, many1, letter )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 letter

------------ TYPES ------------
type Input = String

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Finds a marker (first occurrence of a sequence of distinct characters of a certain size)
-- Takes a marker length and an input string
findMarker :: Int -> String -> Int
findMarker markerLength = findMarker' markerLength markerLength -- We start the string position after the first `markerLength` characters
  where
    findMarker' :: Int -> Int -> String -> Int
    findMarker' markerLength stringPosition string =
      if (length . nub . take markerLength $ string) == markerLength
        then stringPosition
        else findMarker' markerLength (stringPosition + 1) (tail string)

partA :: Input -> OutputA
partA = findMarker 4

------------ PART B ------------
partB :: Input -> OutputB
partB = findMarker 14
