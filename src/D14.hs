module D14 ( d14 ) where

import Utils ( both, prepAnswersS, Answers, StateParser )

import Control.Lens ( (<%~), (%=), (%~), makeLenses )
import Control.Monad.ST ( ST, runST )
import Data.Array.ST ( STUArray, readArray, writeArray, thaw, freeze )
import Data.Array.Unboxed ( UArray, inRange, array, bounds, )
import Data.Foldable.Extra ( findM )
import Data.List ( head, tail )
import Prelude hiding ( some, head, tail )
import Relude.Extra ( dup )
import Text.Megaparsec ( some )
import Text.Megaparsec.Char ( char, newline, string )
import qualified Data.Set as S
import qualified Text.Megaparsec.Char.Lexer as L

type Point   = (Int, Int)
type Vacant  = Bool
type Settled = Int
data PState  = PS { _minX  :: Int, _maxX  :: Int, _maxY  :: Int, _occed :: Set (Int, Int) }
makeLenses ''PState

xOrig :: Int
xOrig = 500

-- *** SOLUTION *** --
d14 :: FilePath -> Text -> Answers
d14 = prepAnswersS solve sigP $ PS xOrig xOrig 0 S.empty
  where solve = both (show . fall) . bimap mkArray mkArray2 . dup

fall :: UArray Point Vacant -> Settled
fall ia = runST $ go 0 [] (xOrig, 0) ia
  where
    go :: Settled -> [Point] -> Point -> UArray Point Vacant -> ST s Settled
    go stld prevs cur@(x, y) ar = do
      stA <- thaw ar :: ST s (STUArray s (Int, Int) Vacant)
      if all (inRange $ bounds ar) candidates then do
        mNxt <- findM (readArray stA) candidates -- select the first vacant candidate point
        maybe (settle stA) (\nxt -> freeze stA >>= go stld (cur : prevs) nxt) mNxt
        else pure stld -- unit fell into abyss
      where
        candidates = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
        settle stA = do
          ar' <- writeArray stA cur False >> freeze stA
          if null prevs then pure (stld + 1) else go (stld + 1) (tail prevs) (head prevs) ar'

-- *** PARSER *** --
sigP :: StateParser PState PState -- (UArray (Int, Int) Vacant)
sigP = some (some coordsP <* newline >>= occLines) >> get
  where
    coordsP = do
      x <- L.decimal <* char ','
      y <- L.decimal <* optional (string " -> ")
      minX %= min x >> maxX %= max x >> maxY %= max y >> pure (x, y) -- update coordinate fields

-- *** HELPERS *** --
mkArray, mkArray2 :: PState -> UArray Point Vacant
mkArray (PS x0 x' y' o) = array ((x0, 0), (x', y')) $ map (, False) (S.toList o) ++ blanks
  where blanks = [ ((x, y), True) | x <- [x0 .. x'], y <- [0 .. y'], (x, y) `S.notMember` o ]
-- adjust PState values for Part 2, increasing array dimensions and adding floor
mkArray2 pS0 = mkArray $ pS3 & occed %~ (`S.union` S.fromList [ (x, y') | x <- [x0 .. x'] ])
  where -- adjust maxY, minX and maxX
    (y', pS1) = pS0 & maxY <%~ (+ 2)
    (x0, pS2) = pS1 & minX <%~ min (xOrig - y')
    (x', pS3) = pS2 & maxX <%~ max (xOrig + y')

occLines :: MonadState PState m => [Point] -> m ()
occLines (p1:p2:ps) = occLine p1 p2 >> occLines (p2:ps)
occLines _          = pure ()

occLine :: MonadState PState m => Point -> Point -> m ()
occLine (x1, y1) (x2, y2)
  | x1 == x2  = addOcced $ fmap (x1, ) (mkRange y1 y2)
  | y1 == y2  = addOcced $ fmap (, y1) (mkRange x1 x2)
  | otherwise = pure ()
  where
    addOcced    = (occed %=) . flip S.union . S.fromList
    mkRange a b = if a <= b then [a .. b] else [a, a - 1 .. b]