module D01 ( d01 ) where

import Utils ( Parser, prepAnswers2 )

import Data.List ( insertBy )
import Text.Megaparsec ( eof, sepEndBy )
import Text.Megaparsec.Char ( newline )
import qualified Text.Megaparsec.Char.Lexer as L

type ElfSnacks = [Int]

-- *** SOLUTION *** --
d01 :: FilePath -> Text -> (String, String)
d01 = prepAnswers2 getMaxCals getTop3Cals elfSnacksParser

getMaxCals :: [ElfSnacks] -> String
getMaxCals = show . foldr (\elf maxCals -> max maxCals (sum elf)) 0

getTop3Cals :: [ElfSnacks] -> String
getTop3Cals = show . sum . foldr (\elf top3 -> take 3 $ insertBy (flip compare) (sum elf) top3) []

-- *** PARSER *** --
elfSnacksParser :: Parser [ElfSnacks]
elfSnacksParser = sepEndBy (many (L.decimal <* newline)) newline <* eof
