module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text ( Parser )
import Util.Parsers (coordinateParser)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = coordinateParser (Just . read . pure) 0

------------ TYPES ------------
type TreePatch = Map (Int, Int) Int

type Input = TreePatch

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Given a tree patch and a list of co-ordinates, return a set of trees which you could see when looking "along" the list of co-ordinates
lookAtForestInDirection :: TreePatch -> [(Int, Int)] -> Int -> Set (Int, Int) -> Set (Int, Int)
lookAtForestInDirection _ [] _ peaks = peaks
lookAtForestInDirection trees (pos : rest) highest peaks =
  -- We can only see a tree if it's higher than all the trees in front of it
  if trees Map.! pos > highest
    then lookAtForestInDirection trees rest (trees Map.! pos) (Set.insert pos peaks)
    else lookAtForestInDirection trees rest highest peaks

-- Look along all possible axes and call `viewInDirection` on each
partA :: Input -> OutputA
partA trees =
  let (xMin, xMax, yMin, yMax) = U.mapBoundingBox trees
   in Set.size . Set.unions $
        [lookAtForestInDirection trees [(x, y) | y <- [yMin .. yMax]] (-1) Set.empty | x <- [xMin .. xMax]]
          ++ [lookAtForestInDirection trees [(x, y) | y <- [yMax, yMax - 1 .. yMin]] (-1) Set.empty | x <- [xMin .. xMax]]
          ++ [lookAtForestInDirection trees [(x, y) | x <- [xMin .. xMax]] (-1) Set.empty | y <- [yMin .. yMax]]
          ++ [lookAtForestInDirection trees [(x, y) | x <- [xMax, xMax - 1 .. xMin]] (-1) Set.empty | y <- [yMin .. yMax]]

------------ PART B ------------
-- Given a tree patch and a list of positions, determine "how far" you could look along that list of positions without the view being blocked
viewDistance :: TreePatch -> [(Int, Int)] -> Int
viewDistance trees positions = viewDistance' 0 (trees Map.! head positions) (tail positions)
  where
    viewDistance' count _ [] = count
    viewDistance' count height (pos : rest) =
      if (trees Map.! pos) < height
        then viewDistance' (count + 1) height rest
        else count + 1

-- Look in each of the four cardinal directions and compute the "scenic score" by multiplying the view distances together
partB :: Input -> OutputB
partB trees = maximum . Map.elems $ Map.mapWithKey scenicScore trees
  where
    (xMin, xMax, yMin, yMax) = U.mapBoundingBox trees
    scenicScore (x, y) h =
      viewDistance trees [(x, y') | y' <- [y .. yMax]]
        * viewDistance trees [(x, y') | y' <- [y, y - 1 .. yMin]]
        * viewDistance trees [(x', y) | x' <- [x .. xMax]]
        * viewDistance trees [(x', y) | x' <- [x, x - 1 .. xMin]]
