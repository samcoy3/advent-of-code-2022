module Days.Day09 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
    ( (++),
      length,
      foldr1,
      foldl',
      last,
      replicate,
      reverse,
      nub,
      scanl' )

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( sepBy, decimal, space, char, endOfLine, Parser )
import Util.Pair ( Pair(Pair), (<+>), (<->) )
import Data.Functor (($>), (<&>))
import Control.Applicative ((<|>))
import Data.Function ((&))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = foldr1 (++) <$> move `sepBy` endOfLine
  where
    move = do
      direction <-
        char 'R' $> Pair (1, 0)
          <|> char 'D' $> Pair (0, -1)
          <|> char 'L' $> Pair (-1, 0)
          <|> char 'U' $> Pair (0, 1)
      space
      count <- decimal
      return (replicate count direction)

------------ TYPES ------------
-- Reading the input as a list of single moves
-- e.g. "U 5" corresponds to five up moves
-- This could be costly (e.g. R 1000) but it happens not to be
type Input = [Pair Int]

data RopeA = RopeA {ropeHead :: Pair Int, ropeTail :: Pair Int}
  deriving (Show)

type OutputA = Int

type OutputB = Int

------------ PART A ------------
-- Applies a move to the head of the rope and propagates it to the tail
moveRopeA :: Pair Int -> RopeA -> RopeA
moveRopeA move RopeA {..} =
  let newHead = ropeHead <+> move
      newTail = moveCloser ropeTail newHead
   in RopeA {ropeHead = newHead, ropeTail = newTail}

-- Moves a "follower" section of rope closer to a "leader" section of rope,
--   if the follower is far enough away (as per the question)
moveCloser :: Pair Int -> Pair Int -> Pair Int
moveCloser follower@(Pair (fx, fy)) leader@(Pair (lx, ly)) =
  let delta@(Pair (dx, dy)) = abs <$> (leader <-> follower)
   in -- check if distance is >=2 in either direction
      -- if so, move both co-ordinates of follower closer to leader (if possible)
      if dx > 1 || dy > 1
        then Pair (adjust fx lx, adjust fy ly)
        else follower
  where
    -- `adjust` moves `a` one step closer to `b`
    adjust a b =
      if
          | a < b -> a + 1
          | a > b -> a - 1
          | otherwise -> a

-- Apply the sequence of moves to the original rope in order, map to get the tail positions,
--   and find how many unique positions there are
partA :: Input -> OutputA
partA moves =
  length
    . nub
    . fmap ropeTail
    . scanl' (&) (RopeA (Pair (0, 0)) (Pair (0, 0)))
    $ moveRopeA <$> moves

------------ PART B ------------
type RopeB = [Pair Int]

-- Similar to `moveRopeA`, but using a fold to propagate the movement along the list of segments
moveRopeB :: Pair Int -> RopeB -> RopeB
moveRopeB move (head : rest) =
  reverse $ foldl' propagateMove [head <+> move] rest
  where
    propagateMove rope@(prevSegment : _) nextSegment = moveCloser nextSegment prevSegment : rope

partB :: Input -> OutputB
partB moves =
  length
    . nub
    . fmap last
    . scanl' (&) (replicate 10 $ Pair (0, 0))
    $ moveRopeB <$> moves
