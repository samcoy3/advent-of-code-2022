module Days.Day12 (runDay) where

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
inputParser = coordinateParser Just 0

------------ TYPES ------------
-- A map from a 2D co-ordinate to the height of the point
-- Heights are characters in a..z, with z being higher than a.
type HeightMap = Map (Int, Int) Char

type Input = HeightMap

type OutputA = Int

type OutputB = Int

------------ PART A ------------
neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

-- Determines whether it is valid to move from one char to another.
-- For the purposes of the question S has elevation 'a', and E has elevation 'z'.
moveable :: Char -> Char -> Bool
moveable 'S' c = moveable 'a' c
moveable c 'E' = moveable c 'z'
moveable c1 c2 = c2 <= succ c1

{-
Runs a BFS, returning the distance from some source to the closest point in a set of targets.
Takes four parameters:
- `heightMap`: Our input HeightMap
- `validMove`: A function which takes two chars and returns True iff going from the first to the second is a valid move
- `source`: The starting node of the BFS
- `targets`: The set of targets: when the BFS reaches a node in this set, it terminates
-}
bfs ::
  HeightMap ->
  (Char -> Char -> Bool) ->
  (Int, Int) ->
  Set (Int, Int) ->
  Int
bfs heightMap validMove source targets =
  bfs' (Set.singleton source) (Set.singleton source) targets 0
  where
    -- `explored` is the set of points which we have explored
    -- `frontier` is a subset of `explored` and is the set of points which we explored last round
    -- `count` is our current distance from the source
    bfs' explored frontier targets count =
      -- If one of our targets has been explored, then we are done
      if not . Set.null $ targets `Set.intersection` frontier
        then count
        else -- We compute a new frontier, by:
        -- - Taking the previous frontier, and computing all its neighbours
        -- - Filtering this list to neighbours which we can legally move to
        -- - Removing all nodes which we have already explored in a previous round

          let newFrontier =
                (`Set.difference` explored)
                  . Set.unions
                  . Set.map
                    ( \p ->
                        Set.fromList $
                          filter
                            ( \p' ->
                                Map.member p' heightMap
                                  && validMove (heightMap Map.! p) (heightMap Map.! p')
                            )
                            (neighbours p)
                    )
                  $ frontier
           in bfs' (Set.union explored newFrontier) newFrontier targets (count + 1)

-- A function which takes a character and a HeightMap, and returns the location of the character in the HeightMap
-- We promise that the character appears exactly once in the HeightMap (i.e. it is the source or target)
findSquare :: Char -> HeightMap -> (Int, Int)
findSquare c = head . Map.keys . Map.filter (== c)

partA :: Input -> OutputA
partA heightMap =
  let source = findSquare 'S' heightMap
      target = findSquare 'E' heightMap
   in bfs heightMap moveable source (Set.singleton target)

------------ PART B ------------
-- We note that the question is equivalent to finding the closest point of elevation 'a' from the target,
--   provided the rule for "valid moves" is reversed: i.e. we "climb backwards"
partB :: Input -> OutputB
partB heightMap =
  let pointsWithElevationA = Set.fromList . Map.keys $ Map.filter (== 'a') heightMap
      source = findSquare 'E' heightMap
   in bfs heightMap (flip moveable) source pointsWithElevationA
