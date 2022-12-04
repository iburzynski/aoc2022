module D04 where

import qualified Text.Megaparsec.Char.Lexer as L

import Utils ( sumAnswers, Parser, prepAnswers )
import Text.Megaparsec.Char ( char, newline )
import Relude.Extra ( dup )

type Range   = (Int, Int)
type ElfPair = (Range, Range)

data IntervalMode = Contains | Overlaps deriving Eq

-- *** SOLUTION *** --
d04 :: FilePath -> Text -> (String, String)
d04 = prepAnswers (sumAnswers (scorePair Contains) (scorePair Overlaps)) pairsP

-- score 1 if predicate is satisfied in either direction, else 0
scorePair :: IntervalMode -> ElfPair -> Int
scorePair m = fromEnum . uncurry (||) . bimap (makePredicate m) (makePredicate m . swap) . dup

makePredicate :: IntervalMode -> ElfPair -> Bool
makePredicate mode p = l2 >= l1 && u1 >= x
  where
    ((l1, u1), (l2, u2)) = p
    x = if mode == Contains then u2 else l2

-- *** PARSER *** --
pairsP :: Parser [ElfPair]
pairsP = many $ do
  e1 <- rangeP
  _  <- char ','
  e2 <- rangeP
  _  <- newline
  pure (e1, e2)

rangeP :: Parser (Int, Int)
rangeP = do
  l <- L.decimal
  _ <- char '-'
  u <- L.decimal
  if u < l then fail "Invalid range" else pure (l, u)