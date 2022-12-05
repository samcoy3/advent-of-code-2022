module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List ( foldl', transpose )
import Data.Maybe ( catMaybes )
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( Parser,
      count,
      many1,
      sepBy,
      decimal,
      space,
      anyChar,
      char,
      endOfLine,
      string )
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Control.Monad (void)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (,) <$> stacks <*> instructions
  where
    -- Parses our list of stacks
    -- Takes the stack description row by row, then transposes it and removes the empty spaces
    stacks =
      (Vec.fromList . fmap catMaybes . transpose <$> (row `sepBy` endOfLine))
        <* many1 (void space <|> void decimal) -- Skipping the numbers at the bottom of the stacks, and the newlines

    -- Parses a row of the stack description
    row =
      ( (count 3 (char ' ') $> Nothing)
          <|> (char '[' *> (Just <$> anyChar) <* char ']')
      )
        `sepBy` char ' '
    instructions = instruction `sepBy` endOfLine
    instruction = do
      string "move "
      quant <- decimal
      string " from "
      from <- decimal
      string " to "
      to <- decimal
      return (quant, from - 1, to - 1) -- 0-indexing the source and target stacks

------------ TYPES ------------
-- Each stack will be a list of chars (crates), with the "top" crate first in the list
type Stack = [Char]

-- The instruction is a triple consisting of:
-- - The quantity to be moved
-- - The source stack (0-indexed)
-- - The target stack (0-indexed)
type Instruction = (Int, Int, Int)

-- Our input will be a vector of the input stacks, and a list of instructions
type Input = (Vector Stack, [Instruction])

type OutputA = String

type OutputB = String

------------ PART A ------------
-- A function to apply a single instruction to our vector of stacks
-- Takes a boolean to determine whether to reverse the crates being moved or not,
--   an input stack, and an instruction
applyInstruction :: Bool -> Vector Stack -> Instruction -> Vector Stack
applyInstruction reverseCrates stacks (quant, from, to) =
  let cratesToMove = take quant (stacks Vec.! from) -- The crates being transferred
   in stacks
        Vec.// [ (from, drop quant $ stacks Vec.! from), -- Remove crates from the source stack
                 ( to, -- Put crates on the target stack
                   (if reverseCrates then reverse cratesToMove else cratesToMove)
                     ++ stacks Vec.! to
                 )
               ]

partA :: Input -> OutputA
partA (stacks, instructions) =
  Vec.toList . fmap head $
    foldl' (applyInstruction True) stacks instructions

------------ PART B ------------
partB :: Input -> OutputB
partB (stacks, instructions) =
  Vec.toList . fmap head $
    foldl' (applyInstruction False) stacks instructions
