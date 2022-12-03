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

type Parser = Parsec Void Text

prepAnswers :: Parser a -> (a -> String) -> (a -> String) -> FilePath -> Text -> (String, String)
prepAnswers p f g fp t = case runParser p fp t of
  Right x -> (f x, g x)
  Left e  -> (errorBundlePretty e, "")

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

maybeToEither :: String -> Maybe a -> Either String a
maybeToEither e mx = case mx of
  Just x -> Right x
  Nothing -> Left e

-- converts error type of parse result to String for composability
convertError :: (VisualStream s, TraversableStream s, ShowErrorComponent e)
  => Either (ParseErrorBundle s e) a -> Either String a
convertError = first errorBundlePretty