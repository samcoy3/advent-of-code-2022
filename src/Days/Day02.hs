module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}

import qualified Program.RunDay as R (runDay, Day)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text
    ( sepBy, space, char, endOfLine, Parser )
import Data.Functor (($>))
import Util.Parsers (around)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
-- We represent rock by 1, paper by 2, and scissors by 3
-- (this is in keeping with the scoring system in the problem)
inputParser :: Parser Input
inputParser = (move `around` space) `sepBy` endOfLine
  where
    move =
      (char 'A' <|> char 'X') $> 1
        <|> (char 'B' <|> char 'Y') $> 2
        <|> (char 'C' <|> char 'Z') $> 3

------------ TYPES ------------
type Input = [(Int, Int)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
{-
We have the following table for rock-paper-scissors:
(Rock, paper, scissors abreviated to R, P, S resp.)
(Win, draw, loss abbreviated to W, D, L resp.)

        Oppo.
      | R P S
    --+-------
    R | D L W
You P | W D L
    S | L W D

Notice that this structure maps nicely onto subtraction modulo 3:
  when we take the move that is "one higher" than our opponent we win,
  when we take the move "one lower", we lose.

We then need to add one to the subtraction (mod 3) to get 0 points for a loss,
  1 for a draw, and 2 for a win (and we then multiply this all by 3), to get:
-}
partA :: Input -> OutputA
partA = sum . fmap score
  where
    score (opponent, you) =
      (((1 + you - opponent) `mod` 3) * 3) -- Score for the result
        + you -- Score for our move

------------ PART B ------------
{-
This works for a very similar reason to part A, informally we can rearrange

  you - opponent = result (mod 3)

  to

  you = opponent + result (mod 3)

  though some additional care is needed to handle the off-by-one errors
  which result from treating a loss as 1 point in the input and 0 (times 3)
  points in the output.
-}
partB :: Input -> OutputB
partB = sum . fmap score
  where
    score (opponent, result) =
      (((opponent + result) `mod` 3) + 1) -- Score for our move
        + ((result - 1) * 3) -- Score for the result
