module Utils where

import Text.Megaparsec (
    Parsec
  , ParsecT
  , runParser
  , errorBundlePretty
  , ParseErrorBundle
  , VisualStream
  , TraversableStream
  , ShowErrorComponent
  )
import Relude.Extra ( dup )
import Text.Megaparsec.Char ( char )

type Parser        = Parsec Void Text
type StateParser s = StateT s (ParsecT Void Text Identity)
type Answer        = String
type Answers       = (String, String)

-- *** ANSWER UTILS *** --
prepAnswers :: (a -> Answers) -> Parser a -> FilePath -> Text -> Answers
prepAnswers f p fp t = case runParser p fp t of
  Right x -> f x
  Left e  -> (errorBundlePretty e, "")

prepAnswersS :: (a -> Answers) -> StateParser s a -> s -> FilePath -> Text -> Answers
prepAnswersS f sp s = prepAnswers f (evalStateT sp s)

testParseS :: StateParser s a -> s -> FilePath -> Text -> a
testParseS sp s fp t = case runParser (evalStateT sp s) fp t of
  Right x -> x
  Left e -> error . toText $ errorBundlePretty e

prepAnswers2 :: (a -> Answer) -> (a -> Answer) -> Parser a -> FilePath -> Text -> Answers
prepAnswers2 f g = prepAnswers (bimap f g . dup)

sumAnswers :: (a -> Int) -> (a -> Int) -> [a] -> Answers
sumAnswers f g = both show . foldr (\x -> bimap (+ f x) (+ g x)) (0, 0)

-- *** PARSER UTILS *** --
optSpaceP :: Parser ()
optSpaceP = void $ optional (char ' ')

-- converts error type of parse result to String for composability
convertError :: (VisualStream s, TraversableStream s, ShowErrorComponent e)
  => Either (ParseErrorBundle s e) a -> Either String a
convertError = first errorBundlePretty

-- *** MISC. UTILS *** --
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither e mx = case mx of
  Just x -> Right x
  Nothing -> Left e

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead [] = Nothing

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f x = f x
applyWhen _    _ x = x

fromEither :: Either a a -> a
fromEither = either id id

toEither :: Bool -> a -> Either a a
toEither b x = if b then Right x else Left x