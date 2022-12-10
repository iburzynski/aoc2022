module Utils where

import Text.Megaparsec (
    Parsec
  , runParser
  , errorBundlePretty
  , ParseErrorBundle
  , VisualStream
  , TraversableStream
  , ShowErrorComponent
  )
import Relude.Extra (dup)
import Text.Megaparsec.Char (char)
import GHC.Arr (Array, array)

type Parser = Parsec Void Text
type Answer = String
type Answers = (String, String)

prepAnswers :: (a -> Answers) -> Parser a -> FilePath -> Text -> Answers
prepAnswers f p fp t = case runParser p fp t of
  Right x -> f x
  Left e  -> (errorBundlePretty e, "")

prepAnswers2 :: (a -> Answer) -> (a -> Answer) -> Parser a -> FilePath -> Text -> Answers
prepAnswers2 f g = prepAnswers (bimap f g . dup)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither e mx = case mx of
  Just x -> Right x
  Nothing -> Left e

sumAnswers :: (a -> Int) -> (a -> Int) -> [a] -> Answers
sumAnswers f g = both show . foldr (\x -> bimap (+ f x) (+ g x)) (0, 0)

-- converts error type of parse result to String for composability
convertError :: (VisualStream s, TraversableStream s, ShowErrorComponent e)
  => Either (ParseErrorBundle s e) a -> Either String a
convertError = first errorBundlePretty

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead [] = Nothing

optSpaceP :: Parser ()
optSpaceP = void $ optional (char ' ')

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f x = f x
applyWhen _    _ x = x

to2DArray :: [[a]] -> Array (Int, Int) a
to2DArray grid@(r:_) = let h = length grid - 1; w = length r - 1 in
  array ((0,0), (w,h)) [((x,y), v) | (y, row) <- zip [0..] grid, (x, v) <- zip [0..] row]
to2DArray [] = array ((0, 0), (-1, -1)) []