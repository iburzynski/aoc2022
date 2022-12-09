module D07 where

import Utils ( Answers, Parser, both, prepAnswers, safeHead )

import Data.Semigroup ( Min, getMin )
import Text.Megaparsec ( choice, try, eof )
import Text.Megaparsec.Char ( alphaNumChar, char, newline, space, space1, string )
import qualified Text.Megaparsec.Char.Lexer as L

data RoseTree a = Leaf a   | Branch [RoseTree a]
data DirContent = File Int | Subdir
type FileTree   = RoseTree Int
type DiskSize   = Int

instance Functor RoseTree where
  fmap f (Leaf x)    = Leaf $ f x
  fmap f (Branch ts) = Branch $ fmap f <$> ts

instance Foldable RoseTree where
  foldr f z (Leaf x)    = f x z
  foldr f z (Branch ts) = foldr (flip $ foldr f) z ts

_MAX_SIZE_, _CAPACITY_, _REQUIRED_ :: Int
_MAX_SIZE_ = 100000
_CAPACITY_ = 70000000
_REQUIRED_ = 30000000

-- *** SOLUTION *** --
d07 :: FilePath -> Text -> Answers
d07 = prepAnswers calcAnswers dirP

calcAnswers :: FileTree -> (String, String)
calcAnswers ft = both ($ sum <$> allTrees ft) (sumSizesBelowMax, getDeletionTarget)

sumSizesBelowMax, getDeletionTarget :: [DiskSize] -> String
sumSizesBelowMax        = show . sum . filter (<= _MAX_SIZE_)
getDeletionTarget sizes = maybe "Invalid input!" (show . getMin) (foldMap go sizes) where
  go :: Int -> Maybe (Min Int)
  go dirSize = do
    root <- safeHead sizes
    if _CAPACITY_  - _REQUIRED_ - root + dirSize >= 0
      then Just $ pure dirSize else Nothing

-- *** HELPERS *** --
allTrees :: FileTree -> [FileTree]
allTrees t = case t of
  Branch ts -> t : concatMap allTrees ts
  _ -> [] -- a leaf has no subtrees

contsToLeaves :: [DirContent] -> [FileTree]
contsToLeaves = foldr go [] where
  go c ts = case c of
    File s -> Leaf s : ts
    _ -> ts

-- *** PARSER *** --
dirP :: Parser FileTree
dirP = do
  fs  <- cdP *> lsP *> contentsP
  sts <- many (try dirP) <* (cdUpP <|> eof)
  pure $ Branch (contsToLeaves fs ++ sts)

cdP :: Parser ()
cdP = cmdP *> string "cd" *> void fileNameP

lsP :: Parser ()
lsP = cmdP *> string "ls" *> void newline

cdUpP :: Parser ()
cdUpP = cmdP *> string "cd" *> space *> string ".." *> void newline

cmdP :: Parser ()
cmdP = char '$' *> space1

contentsP :: Parser [DirContent]
contentsP = many (fileP <|> subdirP)

fileP :: Parser DirContent
fileP = File <$> L.decimal <* fileNameP

subdirP :: Parser DirContent
subdirP = Subdir <$ string "dir" <* fileNameP

fileNameP :: Parser String
fileNameP = do
  n <- space1 *> many (choice [alphaNumChar, char '/', char '.']) <* newline
  guard (n /= "..")
  pure n