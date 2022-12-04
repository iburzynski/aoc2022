module D02 where

import Utils ( Parser, prepAnswers, sumAnswers )

import Relude.Extra ( next, prev )
import Text.Megaparsec ( choice )
import Text.Megaparsec.Char ( char, newline )

data OMove = A | B | C
  deriving (Bounded, Enum, Eq, Read, Show)

data PMove = X | Y | Z
  deriving (Bounded, Enum, Eq, Read, Show)

type Round = (OMove, PMove)
type ScoreMethod = OMove -> PMove -> Int

-- *** SOLUTION *** --
d02 :: FilePath -> Text -> (String, String)
d02 = prepAnswers (sumAnswers method1 method2) rpsParser

method1 :: Round -> Int
method1 (o, p)
  | o' == p' = 3 + p' -- tie
  | (p' + 1) `mod` 3 == o' `mod` 3 = p' -- lose
  | otherwise = 6 + p' -- win
  where
    o' = adjE o
    p' = adjE p

method2 :: Round -> Int
method2 (o, p) = r + case r of
  3 -> o'
  6 -> adjE (next o)
  _ -> adjE (prev o)
  where
    o' = adjE o
    r = fromEnum p * 3

-- *** UTIL *** --

-- Adjust default Enum values from [0, 1, 2] to [1, 2, 3] for scoring
adjE :: Enum a => a -> Int
adjE = succ . fromEnum

-- *** PARSER *** --
rpsParser :: Parser [Round]
rpsParser = many $ do
  o <- choice [A <$ char 'A', B <$ char 'B', C <$ char 'C']
  _ <- char ' '
  p <- choice [X <$ char 'X', Y <$ char 'Y', Z <$ char 'Z']
  _ <- newline
  pure (o, p)