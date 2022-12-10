{-# LANGUAGE FlexibleContexts #-}

module D08 ( d08 ) where

import Utils ( Answers, StateParser, both, applyWhen, toEither, fromEither, prepAnswersS )

import Data.Char ( digitToInt )
import GHC.Arr ( Array, (!), array, bounds, indices )
import Text.Megaparsec.Char ( digitChar, newline )
import Control.Monad ( foldM )
import Text.Megaparsec ( count, eof )
import Relude.Extra (dup)

type Height       = Int
type SightLine    = [Height]
type Tree         = (Int, Int)
type Forest       = Array Tree Height
type VisibleTrees = Int
type ScenicScore  = Int

data ForestState = FState { getCurrIdx :: Tree
                          , getCurrForest :: [(Tree, Height)]
                          } deriving Show

-- *** SOLUTION *** --
d08 :: FilePath -> Text -> Answers
d08 = prepAnswersS solve forestP (FState (0, 0) []) -- initial forest state
  where solve forest = both show . foldr (scoreTree forest) (0, 0) $ indices forest

scoreTree :: Forest -> Tree -> (VisibleTrees, ScenicScore) -> (VisibleTrees, ScenicScore)
scoreTree forest tree (vis, scenic) = ( applyWhen isVis (+ 1) vis
                                      , applyWhen (not $ any null slines) (max scenic') scenic)
  where
    slines = getSightLines forest tree
    -- Part 1: check visibility
    isVis = null slines || not (all (any (>= forest ! tree)) slines)
    -- Part 2: calculate scenic score (prod. of all view dists. for the tree's sight lines)
    scenic' = getProduct $ foldMap (pure . fromEither . foldM procVDist 0) slines
    procVDist vd h = toEither (h < forest ! tree) (vd + 1)

getSightLines :: Forest -> Tree -> [SightLine]
getSightLines forest (x, y)
  | x `elem` [x0, x'] || y `elem` [y0, y'] = [] -- skip edge trees
  | otherwise = map (uncurry (zipWith (curry (forest !)))) -- create lines of indices & get heights
      [ (repeat x, [y - 1, y - 2 .. y0]) -- trees above
      , ([x - 1, x - 2 .. x0], repeat y) -- trees to left
      , ([x + 1 .. x'], repeat y) -- trees to right
      , (repeat x, [y + 1 .. y']) ] -- trees below
  where ((x0, y0), (x', y')) = bounds forest

-- *** PARSER *** --
forestP :: StateParser ForestState Forest
forestP = do
  -- parse first row to determine row length
  (rowLength, _) <- (some heightP <* newline) >> getCurrIdx <$> get
  -- ensure equal number of rows and columns
  _ <- nextRow >> count (rowLength - 1) (rowP rowLength) <* eof
  FState _ f@((uBound, _):_) <- get
  pure $ array (dup 0, uBound) f

heightP :: StateParser ForestState ()
heightP = do
  t <- digitToInt <$> digitChar
  modify (addTree t) -- update the X index
  where addTree h (FState idx forest) = FState { getCurrIdx    = first (+ 1 ) idx
                                               , getCurrForest = (idx, h) : forest }

rowP :: Int -> StateParser ForestState ()
rowP n = (count n heightP <* newline) >> nextRow
-- reset X index and increment Y index to prepare for new row

nextRow :: (Monad m, MonadState ForestState m) => m ()
nextRow = modify (\fState -> fState { getCurrIdx = bimap (const 0) (+ 1) (getCurrIdx fState)})