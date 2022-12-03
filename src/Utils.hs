module Utils where

import Text.Megaparsec (Parsec, runParser, errorBundlePretty)

type Parser = Parsec Void Text

prepAnswers :: Parser a -> (a -> String) -> (a -> String) -> FilePath -> Text -> (String, String)
prepAnswers p f g fp t = case runParser p fp t of
  Right x -> (f x, g x)
  Left e  -> (errorBundlePretty e, "")