module Days.Day17 (runDay) where

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
import Data.Attoparsec.Text hiding (take)
import Data.Void
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Util.Pair (Pair (..), (<+>))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = many1 (("<" $> L) <|> (">" $> R))

------------ TYPES ------------
data Direction = L | R deriving (Show)

type Input = [Direction]

type Point = Pair Int

type Rock = Set Point

type OutputA = Int

type OutputB = Void

------------ PART A ------------
rocks :: [Rock]
rocks =
  cycle $
    fmap
      (Set.fromList . fmap Pair)
      [ [(2, 0), (3, 0), (4, 0), (5, 0)],
        [(2, 1), (3, 1), (4, 1), (3, 0), (3, 2)],
        [(2, 0), (3, 0), (4, 0), (4, 1), (4, 2)],
        [(2, 0), (2, 1), (2, 2), (2, 3)],
        [(2, 0), (2, 1), (3, 0), (3, 1)]
      ]

outOfBounds :: Rock -> Set Point -> Bool
outOfBounds rock background = flip any rock $
  \rockpoint@(Pair (x, y)) ->
    Set.member rockpoint background
      || x < 0
      || x > 6
      || y <= 0

dropRock :: Rock -> [Direction] -> Set Point -> ([Direction], Set Point)
dropRock rock (d : dirs) background =
  let delta = Pair . (,0) $ case d of
        L -> -1
        R -> 1
      jetPushedRock = Set.map (<+> delta) rock
      rock' =
        if outOfBounds jetPushedRock background
          then rock
          else jetPushedRock
      fallenRock = Set.map (<+> Pair (0, -1)) rock'
   in if outOfBounds fallenRock background
        then (dirs, Set.union rock' background)
        else dropRock fallenRock dirs background

partA :: Input -> OutputA
partA directions =
  maximum
    . Set.map (snd . getPair)
    . snd
    $ foldl'
      (\(dirs, bg) rock -> dropRock (repositionRock rock bg) dirs bg)
      (cycle directions, Set.empty)
      (take 2022 rocks)
  where
    repositionRock rock background =
      let maxY = fromMaybe 0 $ Set.lookupMax . Set.map (snd . getPair) $ background
       in Set.map (<+> Pair (0, maxY + 4)) rock

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
