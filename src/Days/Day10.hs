module Days.Day10 (runDay) where

{- ORMOLU_DISABLE -}
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( sepBy, decimal, signed, endOfLine, string, Parser )
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Function ((&))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = instruction `sepBy` endOfLine
  where
    instruction =
      "noop" $> Noop
        <|> Add <$> (string "addx " >> signed decimal)

------------ TYPES ------------
data Instruction = Noop | Add Int deriving (Show)

type Input = [Instruction]

type OutputA = Int

type OutputB = CRTScreen

------------ PART A ------------
-- Takes a starting value and a list of instructions
-- Returns the (0-indexed) list of values at each cycle
executeInstructions :: Int -> [Instruction] -> [Int]
executeInstructions _ [] = []
executeInstructions current (i : is) = case i of
  Noop -> current : executeInstructions current is
  Add x -> current : current : executeInstructions (current + x) is

-- Computes the list of values at each cycle, finds the cycles we care about,
--   and computes their total signal score by applying the multipliers and summing up
partA :: Input -> OutputA
partA =
  sum
    . zipWith (*) [20, 60 ..]
    . (<$> fmap (flip (!!)) [19, 59 .. 219])
    . (&)
    . executeInstructions 1

------------ PART B ------------
-- A hack to get around the Show instance for String using surrounding quotes and escaped newlines
newtype CRTScreen = CRTScreen String

instance Show CRTScreen where
  show (CRTScreen s) = s

-- Chunks the output values into groups of 40,
--   then checks to see if the CRT would draw each
-- It suffices to check whether the cycle number (mod 40) is within 1 of the computed value
partB :: Input -> OutputB
partB =
  CRTScreen
    . unlines
    . fmap
      ( fmap (\p -> if p then '█' else '.') -- U+2588 "Full block"
          . zipWith (\a b -> abs (a - b) < 2) [0 ..]
      )
    . U.chunksOf 40
    . executeInstructions 1
