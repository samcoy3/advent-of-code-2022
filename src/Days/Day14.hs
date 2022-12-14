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

type Input = Map (Int, Int) Object

type OutputA = Void

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
