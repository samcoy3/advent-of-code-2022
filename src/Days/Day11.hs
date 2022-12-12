module Days.Day11 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List ( foldl', sortOn )
import Data.Sequence (Seq (..), (|>))
import qualified Data.Sequence as Seq
import Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( Parser, many1, sepBy, decimal, space, char, endOfLine, string )
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Data.Function ((&))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
{-
An ADT for a Monkey:
  `items` is the list of items which a monkey currently has
  `operation` is the operation which a monkey performs on an item's worry (e.g. `(* 4)`)
  `test` is the number which a monkey divides by to test an item
  `recipients` is a pair of monkey IDs: if the test passes then the item is sent to the first, otherwise it is sent to the second
  `totalInspections` keeps track of the number of items inspected by a monkey
-}
data Monkey = Monkey
  { items :: Seq Int,
    operation :: Int -> Int,
    test :: Int,
    recipients :: (Int, Int),
    totalInspections :: Int
  }

-- Show instance required for the AOC framework (and can't just derive one because of `operation`)
instance Show Monkey where
  show Monkey {..} = "I have " ++ show items ++ "! I've inspected " ++ show totalInspections ++ " items!"

data Part = PartA | PartB

type Input = Vector Monkey

type OutputA = Int

type OutputB = Int

------------ PARSER ------------
-- A slightly involved parser due to the natural-language-y input
inputParser :: Parser Input
inputParser = Vec.fromList <$> monkey `sepBy` endOfLine
  where
    monkey = do
      string "Monkey " >> decimal >> ":" >> many1 space
      string "Starting items: "
      items <- Seq.fromList <$> decimal `sepBy` string ", " <* many1 space
      string "Operation: new = old "
      operation <- monkeyOp <* many1 space
      string "Test: divisible by "
      test <- decimal <* many1 space
      string "If true: throw to monkey "
      recipients <- (,) <$> decimal <*> (many1 space >> string "If false: throw to monkey " >> decimal <* endOfLine)
      return Monkey {totalInspections = 0, ..}
    -- Parses the "operation" part  of the monkey
    monkeyOp = do
      operation <- char '*' $> (*) <|> char '+' $> (+)
      space
      (operation <$> decimal) <|> (string "old" $> (\x -> operation x x))

------------ PART A ------------
{-
Performs one monkey's turn.
Takes as input a Part (A or B) which determines what the monkey does to its item after applying the operation:
- If we're solving it the part A way, then the monkey divides the worry of the item by 3, discarding the remainder
- If we're solving it the part B way, then the numbers could get way too big, very quickly.
  To alleviate this problem the monkey takes the worry of the item modulo the "test divisors" of all monkeys multiplied together.
  This does not affect any of the tests, and ensures the numbers stay within a reasonable range.
Also takes an Int representing the index of the monkey, and the vector of the monkeys.

Performs the turn recursively on the sequence of items which the monkey starts with.
-}
monkeyTurn :: Part -> Int -> Vector Monkey -> Vector Monkey
monkeyTurn part index monkeys =
  if Seq.null . items $ monkeys Vec.! index -- Checks whether the monkey is out of items
    then monkeys
    else
      let -- We get the monkey which is inspecting an item
          inspectingMonkey = monkeys Vec.! index
          -- Deconstruct the monkey's items to find the one it's inspecting now
          (item :<| is) = items inspectingMonkey
          -- Inspect the item:
          -- - First, perform the monkey's operation on the item
          -- - Then, depending on the part (see above), perform an additional operation on the item
          itemAfterInspection =
            ( case part of
                PartA -> (`div` 3)
                PartB -> (`mod` (product . fmap test . Vec.toList $ monkeys))
            )
              . operation inspectingMonkey
              $ item
          -- Determine which monkey is to receive the item
          receivingMonkeyIndex =
            if itemAfterInspection `mod` test inspectingMonkey == 0
              then fst . recipients $ inspectingMonkey
              else snd . recipients $ inspectingMonkey
          receivingMonkey = monkeys Vec.! receivingMonkeyIndex
       in -- Call `monkeyTurn` recursively on the rest of the sequence, updating the vector of monkeys to reflect the moved item
          monkeyTurn part index $
            monkeys
              Vec.// [ ( index,
                         inspectingMonkey
                           { items = is,
                             totalInspections = totalInspections inspectingMonkey + 1
                           }
                       ),
                       ( receivingMonkeyIndex,
                         receivingMonkey
                           { items = items receivingMonkey |> itemAfterInspection
                           }
                       )
                     ]

-- Runs one round of the monkey game (i.e. one turn for each monkey, in order)
performRound :: Part -> Vector Monkey -> Vector Monkey
performRound part monkeys =
  foldl' (&) monkeys $
    monkeyTurn part <$> [0 .. Vec.length monkeys - 1]

-- Computes the "monkey business" (product of the inspections of the busiest two monkeys) after a number of rounds
-- Also takes as input a "Part" indicator (see above)
monkeyBusinessAfterRounds :: Part -> Int -> Vector Monkey -> Int
monkeyBusinessAfterRounds part rounds =
  (\(a : b : _) -> a * b)
    . sortOn negate
    . fmap totalInspections
    . Vec.toList
    . (!! rounds)
    . iterate (performRound part)

partA :: Input -> OutputA
partA = monkeyBusinessAfterRounds PartA 20

------------ PART B ------------
partB :: Input -> OutputB
partB = monkeyBusinessAfterRounds PartB 10_000