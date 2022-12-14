module Days.Day13 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List ( sort )

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( Parser, count, sepBy, decimal, char, endOfLine )
import Control.Applicative.Combinators (between)
import Control.Applicative ((<|>))
import Util.Parsers (around)
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = (packet `around` endOfLine) `sepBy` count 2 endOfLine
  where
    packet =
      Single <$> decimal
        <|> Packet <$> between (char '[') (char ']') (packet `sepBy` char ',')

------------ TYPES ------------
-- An ADT representing a packet
data Packet = Single Int | Packet [Packet]
  deriving (Show)

-- An Eq instance for Packet, needed for Ord
-- Note that we can't just derive one: the instance which GHC would come up with would not be faithful to the question
-- So we define it in turns of our (hand-written) Ord instance
instance Eq Packet where
  (==) x = (== EQ) . compare x

-- Our Ord instance for Packet
-- (mostly just transcribed from the description in the question)
instance Ord Packet where
  compare (Single x) (Single y) = compare x y
  compare (Packet (x : xs)) (Packet (y : ys)) =
    if x == y then compare xs ys else compare x y
  compare (Packet []) (Packet (_ : _)) = LT
  compare (Packet (_ : _)) (Packet []) = GT
  compare (Packet []) (Packet []) = EQ
  compare x@(Packet _) (Single y) = compare x (Packet [Single y])
  compare (Single x) y@(Packet _) = compare (Packet [Single x]) y

type Input = [(Packet, Packet)]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = sum . zipWith (\i (x, y) -> i * fromEnum (x <= y)) [1 ..]

------------ PART B ------------
-- Checks whether our packet is one of the decoder packets
-- Note that this is slightly more precise than checking that the packet is *equal* to a decoder packet
-- (since, e.g., [[6]]===[6], by our notion of equality)
isDecoder :: Packet -> Bool
isDecoder (Packet [Packet [Single x]]) = x `elem` [2, 6]
isDecoder _ = False

partB :: Input -> OutputB
partB =
  product
    . fmap fst
    . filter (isDecoder . snd)
    . zip [1 ..]
    . sort
    . (Packet [Packet [Single 2]] :)
    . (Packet [Packet [Single 6]] :)
    . uncurry (++)
    . unzip
