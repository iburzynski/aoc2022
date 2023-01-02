module D14 ( d14 ) where

import Utils

import Control.Lens
import Data.Foldable.Extra ( findM )
import Prelude hiding ( some )
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Array
import Data.Array.IO ( IOArray )
import Data.Array.MArray ( MArray, readArray, writeArray, getBounds, thaw )
import qualified Data.Set as S
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Semigroup

type Vacant = Bool
data MyState = MyState { _settled :: Int, _arr :: IOArray (Int, Int) Vacant }
makeLenses ''MyState

data PState = PS {
    _minX  :: Min Int
  , _maxX  :: Max Int
  , _maxY  :: Max Int
  , _occupied :: Set (Int, Int) }
makeLenses ''PState

d14 = undefined

solve :: Array (Int, Int) Bool -> IO Int
solve a = do
-- ia <- thaw a
  pure undefined

occLine :: MonadState PState m => (Int, Int) -> (Int, Int) -> m ()
occLine (x1, y1) (x2, y2) = occupied %= (`S.union` S.fromList line)
  where
    line = case (compare x1 x2, compare y1 y2) of
      (EQ, LT) -> (x1,)  <$> [y1 .. y2]
      (EQ, GT) -> (x1,)  <$> [y2, y2 - 1 .. y1]
      (LT, EQ) -> (, y1) <$> [x1 .. x2]
      (GT, EQ) -> (, y1) <$> [x2, x2 - 1 .. x1]
      _        -> []

occLines :: MonadState PState m => [(Int, Int)] -> m ()
occLines (p1:p2:ps) = occLine p1 p2 >> occLines (p2:ps)
occLines _ = pure ()

fall :: (MonadIO m, MonadState MyState m, MArray IOArray Vacant m) => (Int, Int) -> m ()
fall p@(x, y) = do
  ar <- use arr
  ((x0, y0), (x', y')) <- liftIO $ getBounds ar
  let inBounds (a, b) = and [a >= x0, a <= x', b >= y0, b <= y']
      candidates      = filter inBounds [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
  unless (null candidates) $ do
    -- helper functions:
    let write = writeArray ar
        again act point b = act >> write point b >> fall point
    mp' <- findM (readArray ar) candidates -- get first vacant position from candidate list
    case mp' of
      -- settled: inc counter, produce new grain at source and start falling
      Nothing -> again (settled %= (+ 1)) (500, 0) True
      -- not settled: vacate current position and fall to new one
      Just p' -> again (write p True)     p'       False

-- *** PARSER *** --
sigP :: StateParser PState (Array (Int, Int) Bool)
sigP = do
  _ <- some linesP
  PS (Min xMin) (Max xMax) (Max yMax) occ <- get
  let blanks = [ ((x, y), True) | x <- [xMin .. xMax], y <- [0 .. yMax], (x, y) `S.notMember` occ ]
  pure $ array ((xMin, 0), (xMax, yMax)) $ map (, False) (S.toList occ) ++ blanks

linesP :: StateParser PState ()
linesP = do
  ls <- some (do
    x <- L.decimal <* char ','
    y <- L.decimal <* optional (string " -> ")
    minX %= (<> Min x)
    maxX %= (<> Max x)
    maxY %= (<> Max y)
    pure (x, y)) <* newline
  occLines ls