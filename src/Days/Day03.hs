module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Data.Set as Set

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( many1, sepBy, letter, endOfLine, Parser )
import Control.Applicative (many)
import Data.Char ( ord, isAlpha, isLower, isUpper )
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 letter `sepBy` endOfLine

------------ TYPES ------------
type Input = [String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Gets the priority for letters as defined in the problem
priority :: Char -> Int
priority c =
  if
      | isAlpha c && isLower c -> ord c - 96
      | isAlpha c && isUpper c -> ord c - 38
      | otherwise -> error "Input character was not in a-zA-Z!"

-- Split the input string for each line in half,
--   then find the character in common between the halves
--   and sum the priorities
partA :: Input -> OutputA
partA = sum . fmap (priority . difference . (\s -> splitAt (length s `div` 2) s))
  where
    difference (s1, s2) =
      minimum $
        Set.intersection (Set.fromList s1) (Set.fromList s2)

------------ PART B ------------
-- Recursively take the input lines in threes,
--   finding the common character and
--   summing priorities as before
partB :: Input -> OutputB
partB = getBadges
  where
    getBadges [] = 0
    getBadges (s1 : s2 : s3 : ss) =
      getBadges ss
        + ( priority . minimum . foldr1 Set.intersection $
              [Set.fromList s1, Set.fromList s2, Set.fromList s3]
          )
