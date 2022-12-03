module D01 where

import Utils ( Parser, prepAnswers )

import Data.List ( insertBy )
import Text.Megaparsec ( eof, sepEndBy )
import Text.Megaparsec.Char ( newline )
import qualified Text.Megaparsec.Char.Lexer as L

elfSnacksParser :: Parser [[Int]]
elfSnacksParser = sepEndBy (many (L.decimal <* newline)) newline <* eof

d01 :: FilePath -> Text -> (String, String)
d01 = prepAnswers elfSnacksParser d01_1 d01_2

d01_1 :: [[Int]] -> String
d01_1 = show . foldr (\elf maxCals -> max maxCals (sum elf)) 0

d01_2 :: [[Int]] -> String
d01_2 = show . sum . foldr (\elf top3 -> take 3 $ insertBy (flip compare) (sum elf) top3) []