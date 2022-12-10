module D08 where

import Utils ( Answers, Parser, to2DArray, both, applyWhen, prepAnswers )

import Data.Char ( digitToInt )
import GHC.Arr ( Array, (!), bounds, indices )
import Text.Megaparsec.Char ( digitChar, newline )
import Control.Monad ( foldM )

type Tree = (Int, Int)
type Height = Int
type SightLine = [Height]
type ViewDist = Int
type VisibleTrees = Int
type ScenicScore = Int
type Forest = Array Tree Height

d08 :: FilePath -> Text -> Answers
d08 = prepAnswers solve gridP

solve :: Forest -> Answers
solve forest = both show . foldr (scoreTree forest) (0, 0) $ indices forest

tf :: Forest
tf = to2DArray [[3,0,3,7,3]
               ,[2,5,5,1,2]
               ,[6,5,3,3,2]
               ,[3,3,5,4,9]
               ,[3,5,3,9,0]]

scoreTree :: Forest -> Tree -> (VisibleTrees, ScenicScore) -> (VisibleTrees, ScenicScore)
scoreTree forest tree (vis, scenic) = ( applyWhen isVis (+1) vis
                                      , applyWhen (not $ any null slines) (max scenic') scenic)
  where
    slines = getSightLines forest tree
    -- Part 1: check visibility
    isVis = null slines || not (all (any (>= forest ! tree)) slines)
    -- Part 2: calculate scenic score (prod. of all view dists. for the tree's sight lines)
      -- if final view distance is a Right value, subtract 1 to remove the edge
    scenic' = getProduct $ foldMap (pure . either id rmvEdge . foldM procVDist 1) slines
    procVDist vd h = if h < forest ! tree then Right $ vd + 1 else Left vd
    rmvEdge = subtract 1

getSightLines :: Forest -> Tree -> [SightLine]
getSightLines arr (x, y)
  | x `elem` [x0, x'] || y `elem` [y0, y'] = [] -- skip edge trees
  | otherwise = [us, ls, rs, ds]
  where
    ((x0, y0), (x', y')) = bounds arr
    getHeights = zipWith (curry (arr !))
    us = getHeights (repeat x) [y - 1, y - 2 .. y0]
    ls = getHeights [x - 1, x - 2 .. x0] (repeat y)
    rs = getHeights [x + 1 .. x'] (repeat y)
    ds = getHeights (repeat x) [y + 1 .. y']

-- *** PARSER *** --
gridP :: Parser Forest
gridP = do
  ds <- many (many digitChar <* newline)
  pure . to2DArray $ map (map digitToInt) ds