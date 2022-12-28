module D03 ( d03 ) where

import Utils ( convertError, both, maybeToEither, Parser )

import Text.Megaparsec ( count, runParser )
import Text.Megaparsec.Char ( letterChar, newline )
import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Item        = Char
type Priority    = Int
type Compartment = Set Item
type Rucksack    = (Compartment, Compartment)
type Group       = [Rucksack]

d03 :: FilePath -> Text -> (String, String)
d03 fp t = case convertError (runParser rucksacksP fp t) >>= processGroups of
  Right ans -> ans
  Left e    -> (e, "")

processGroups :: [Group] -> Either String (String, String)
processGroups gs = both (show . getSum) <$> foldr reducer (Right (Sum 0, Sum 0)) gs
  where
    -- get sums for group and add to accumulated total
    reducer g acc = fmap (<>) (sums g) <*> acc
    -- apply method1 and method2 to group & convert results to `Either String (Sum Int, Sum Int)`
    sums g = bisequence $ both (fmap Sum . maybeToEither (err g) . ($ g)) (method1, method2)
    err x  = "Invalid entry in group: " ++ show x

-- for each bag, find item the compartments have in common and determine its priority; sum all
method1 :: [Rucksack] -> Maybe Int
method1 = fmap sum . traverse (getPriority <=< getItem . uncurry S.intersection)

-- find the item the group has in common and determine its priority
method2 :: Group -> Maybe Priority
method2 []      = Nothing
method2 (r0:rs) = getItem result >>= getPriority
  where
    mergeCs = uncurry S.union  -- combine rucksack compartments
    result = foldr (\r acc -> S.intersection acc (mergeCs r)) (mergeCs r0) rs

-- *** HELPERS *** --
getItem :: Set Item -> Maybe Item
getItem s = case toList s of
  [i] -> Just i -- there should be only one item left in the set
  _   -> Nothing

getPriority :: Item -> Maybe Priority
getPriority = flip M.lookup (M.fromList $ zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1..])

-- *** PARSER *** --
rucksacksP :: Parser [Group]
rucksacksP = many . count 3 $ do -- parse text into groups of 3
  xs <- many letterChar <* newline
  -- split into two compartments
  pure . both S.fromList $ splitAt (length xs `div` 2) xs