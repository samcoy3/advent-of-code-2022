module Days.Day07 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List ( foldl', find, delete )
import Data.Maybe ( fromJust, isNothing )

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
    ( letter,
      many1,
      Parser,
      sepBy,
      decimal,
      space,
      char,
      endOfLine,
      string )
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Data.Function ((&))
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ TYPES ------------
-- An data type representing either a file or a directory
data FileObject
  = File {name :: String, size :: Int}
  | Directory {name :: String, contents :: [FileObject]}
  deriving (Show, Eq)

isDirectory :: FileObject -> Bool
isDirectory Directory {..} = True
isDirectory _ = False

-- A type representing a /local view/ on a file system (like a "zipper list")
--
-- A FileExplorer is always "inside" a directory, a bit like a terminal session:
--   `localContents` represents the contents of the directory which we're in
--   `localDirName` represents the name of the directory which we're in
--   `up` represents the FileExplorer corresponding to our parent directory, with one change:
--     we omit the directory which we are currently in from the `localContents` of `up`
--     (otherwise we have infinite mutual recursion)
--   (N.B. if we're in the root, this is Nothing)
data FileExplorer = FileExplorer
  { up :: Maybe FileExplorer,
    localDirName :: String,
    localContents :: [FileObject]
  }
  deriving (Show)

-- An "empty" FileExplorer, corresponding to a root ("/") folder with no contents
defaultFileExplorer :: FileExplorer
defaultFileExplorer = FileExplorer Nothing "/" []

-- A function which "moves" the FileExplorer to its parent
cdUp :: FileExplorer -> FileExplorer
cdUp f = case up f of
  Nothing -> f
  Just up -> up {localContents = Directory (localDirName f) (localContents f) : localContents up}

-- A function which moves the FileExplorer to a subdirectory, specified by a string
cdToDir :: String -> FileExplorer -> FileExplorer
cdToDir dir f =
  let dirObject = fromJust $ find (\fd -> isDirectory fd && name fd == dir) (localContents f)
   in FileExplorer
        { up = Just (f {localContents = delete dirObject $ localContents f}),
          localDirName = dir,
          localContents = contents dirObject
        }

-- A function which populates the current location of the FileExplorer with a list of contents
setContentsOfCurrentDir :: [FileObject] -> FileExplorer -> FileExplorer
setContentsOfCurrentDir fd f = f {localContents = fd}

type Input = FileExplorer

type OutputA = Int

type OutputB = Int

------------ PARSER ------------
-- The parsing strategy here is to interpret the input as a list of (FileExplorer -> FileExplorer) functions,
--   then apply them in order
inputParser :: Parser Input
inputParser =
  until (isNothing . up) cdUp -- Move the FileExplorer back to "/" when populated
    . foldl' (&) defaultFileExplorer -- Apply all the modifications to the default FileExplorer, in order
    <$> commands
  where
    -- The first command is not relevant (we assume we start there), so we skip it
    commands = string "$ cd /" >> endOfLine >> command `sepBy` endOfLine
    command =
      (string "$ cd .." $> cdUp)
        <|> (string "$ cd " >> (cdToDir <$> many1 letter))
        <|> (string "$ ls" >> endOfLine >> (setContentsOfCurrentDir <$> fileOrDirectory `sepBy` endOfLine))
    fileOrDirectory = file <|> directory
    file = do
      size <- decimal
      space
      name <- many1 (letter <|> char '.')
      return $ File name size
    directory = string "dir " >> (flip Directory [] <$> many1 letter)

------------ PART A ------------
-- We find the total sizes of our directories (recursively)
-- Returns a pair:
-- - the first element is an int which represents the size of the directory corresponding to the FileExplorer
-- - the second element is a list of integers which represent the sizes of all directories in the subtree of the directory corresponding to the FileExplorer
findTotalSizes :: FileExplorer -> (Int, [Int])
findTotalSizes f@FileExplorer {..} =
  let (totalSize, sizeOfSubdirectories) =
        -- List concatenation is probably not very efficient here, but the whole solution runs in <0.01s on the input so it's fine
        foldr (\(a, b) (c, d) -> (a + c, b ++ d)) (0, []) $
          ( \fd ->
              ( case fd of
                  File _ size -> (size, [])
                  Directory name _ -> findTotalSizes (cdToDir name f)
              )
          )
            <$> localContents
   in (totalSize, totalSize : sizeOfSubdirectories)

partA :: Input -> OutputA
partA = sum . filter (<= 100_000) . snd . findTotalSizes

------------ PART B ------------
partB :: Input -> OutputB
partB f =
  let (totalSize, listOfSizes) = findTotalSizes f
   in minimum . filter (>= (totalSize - 40_000_000)) $ listOfSizes
