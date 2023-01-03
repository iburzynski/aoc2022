module D14 ( d14 ) where

import Utils ( both, prepAnswersS, Answers, StateParser )

import Control.Lens ( (<%~), (%=), (%~), makeLenses )
import Control.Monad.ST ( ST, runST )
import Data.Array.ST ( STUArray, readArray, writeArray, thaw, freeze )
import Data.Array.Unboxed ( inRange, array, bounds, UArray )
import Data.Foldable.Extra ( findM )
import Prelude hiding ( some )
import Relude.Extra ( dup )
import Text.Megaparsec ( some )
import Text.Megaparsec.Char ( char, newline, string )
import qualified Data.Set as S
import qualified Text.Megaparsec.Char.Lexer as L

type Vacant = Bool
data PState = PS {
    _minX  :: Int
  , _maxX  :: Int
  , _maxY  :: Int
  , _occupied :: Set (Int, Int) }
makeLenses ''PState

xOrig :: Int
xOrig = 500

d14 :: FilePath -> Text -> Answers
d14 = prepAnswersS solve sigP $ PS xOrig xOrig 0 S.empty
  where solve = both (show . fall) . bimap mkArray mkArray2 . dup

fall :: UArray (Int, Int) Vacant -> Int
fall ia = runST $ go 0 (xOrig, 0) ia
  where
    go s p@(x, y) ar = do
      stA <- thaw ar :: ST s (STUArray s (Int, Int) Vacant)
      _   <- writeArray stA p False
      let bs = bounds ar
          candidates = filter (inRange bs) [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
      if length candidates < 3 then pure s else do
        mp' <- findM (readArray stA) candidates -- get first vacant position from candidate list
        case mp' of
          -- settled: produce new grain at source, inc counter and start falling
          Nothing -> do
            ar' <- freeze stA
            if p == (xOrig, 0) then pure (s + 1) else go (s + 1) (xOrig, 0) ar'
          -- not settled: vacate current position and fall to new one
          Just p' -> do
            _   <- writeArray stA p True
            ar' <- freeze stA
            go s p' ar'

mkArray, mkArray2 :: PState -> UArray (Int, Int) Vacant
mkArray (PS x0 x' y' o) = array ((x0, 0), (x', y')) $ map (, False) (S.toList o) ++ blanks
  where blanks = [ ((x, y), True) | x <- [x0 .. x'], y <- [0 .. y'], (x, y) `S.notMember` o ]
-- adjust PState values for Part 2, increasing array dimensions and adding floor
mkArray2 pS0 = mkArray $ pS3 & occupied %~ (`S.union` S.fromList [ (x, y') | x <- [x0 .. x'] ])
  where -- adjust maxY, minX and maxX
    (y', pS1) = pS0 & maxY <%~ (+ 2)
    (x0, pS2) = pS1 & minX <%~ min (xOrig - y')
    (x', pS3) = pS2 & maxX <%~ max (xOrig + y')

-- *** PARSER *** --
sigP :: StateParser PState PState -- (UArray (Int, Int) Vacant)
sigP = some linesP >> get
  where
    linesP = do
      ls <- some (do
        x <- L.decimal <* char ','
        y <- L.decimal <* optional (string " -> ")
        minX %= min x >> maxX %= max x >> maxY %= max y
        pure (x, y)) <* newline
      occLines ls

-- *** HELPERS *** --
occLines :: MonadState PState m => [(Int, Int)] -> m ()
occLines (p1:p2:ps) = occLine p1 p2 >> occLines (p2:ps)
occLines _          = pure ()

occLine :: MonadState PState m => (Int, Int) -> (Int, Int) -> m ()
occLine (x1, y1) (x2, y2) = occupied %= (`S.union` S.fromList line)
  where
    line = case (compare x1 x2, compare y1 y2) of
      (EQ, LT) -> (x1,)  <$> [y1 .. y2]
      (EQ, GT) -> (x1,)  <$> [y1, y1 - 1 .. y2]
      (LT, EQ) -> (, y1) <$> [x1 .. x2]
      (GT, EQ) -> (, y1) <$> [x1, x1 - 1 .. x2]
      _        -> []