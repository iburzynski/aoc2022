module D12 ( d12 ) where

import Utils ( prepAnswersS, StateParser, Answers, both )

import Control.Lens ( (.=), (<<%=), (%=), use, makeLenses, Field2(_2), Field1(_1) )
import Data.Array.Unboxed ( UArray, (!), array, bounds )
import Data.Tuple.Extra (uncurry3)
import Prelude hiding ( some )
import Relude.Extra (dup)
import Text.Megaparsec ( count, some, MonadParsec(eof) )
import Text.Megaparsec.Char ( letterChar, newline )
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Set as S

type Height    = Int
type Dist      = Int
type Square    = (Int, Int)
type Start     = Square
type End       = Square
type HeightMap = UArray Square Height

data HMapState = HMState {
    _curSquare :: Square
  , _curGrid   :: [(Square, Height)]
  , _path      :: (Maybe Square, Maybe Square)
  , _lows      :: [Square]
  } deriving Show
makeLenses ''HMapState

-- *** SOLUTION *** --
d12 :: FilePath -> Text -> Answers
d12 = prepAnswersS solve gridP (HMState (0, 0) [] (Nothing, Nothing) []) -- initial hmap state
  where solve = maybe (dup "Error: solution not found") (both show) . uncurry3 getDists

getDists :: [Start] -> End -> HeightMap -> Maybe (Dist, Dist)
getDists (s:as) e hm = do
  sPath  <- go S.empty $ PQ.singleton 0 s -- get shortest path to destination from 'S' square
  aPaths <- go S.empty . PQ.fromList $ map (0,) as -- get shortest path from all 'a' squares
  pure $ min aPaths <$> dup sPath where
    go vis q = do
      ((d, sq), rest) <- PQ.minViewWithKey q
      case (sq == e, sq `S.member` vis) of
        (True, _) -> Just d
        (_, True) -> go vis rest
        _         -> go (S.insert sq vis) . foldr (uncurry PQ.insert) rest $ getNeighbors hm (d, sq)
getDists [] _ _ = Nothing

getNeighbors :: HeightMap -> (Dist, Square) -> [(Dist, Square)]
getNeighbors hm (d, s) = [ (d + 1, f g s) -- eligible neighbor with incremented dist.
                         | f <- [first, second] -- modify x (first) and y (second) coords
                         , g <- [subtract 1, (+ 1)] -- (x - 1), (x + 1), (y - 1), (y + 1)
                         -- neighbor must be in bounds and at most 1 higher
                         , inBounds (f g s) && (hm ! f g s) <= (hm ! s) + 1 ] where
    ((x0, y0), (x', y')) = bounds hm
    inBounds (a, b) = and [a >= x0, a <= x', b >= y0, b <= y']

-- *** PARSER *** --
gridP :: StateParser HMapState ([Square], End, HeightMap)
gridP = do
  -- parse first row to determine row length
  (rowLength, _) <- (some squareP <* newline) >> use curSquare
  -- ensure all rows have equal length
  _ <- nextRow >> some (rowP rowLength) <* eof
  g@((ub, _):_) <- use curGrid
  mp <- bisequence <$> use path
  ls <- use lows
  case mp of
    Nothing -> fail "Missing origin and/or destination"
    Just (start, end) -> pure (start : ls , end, array (dup 0, ub) g)

rowP :: Int -> StateParser HMapState ()
rowP n = (count n squareP <* newline) >> nextRow

-- reset X index and increment Y index to prepare for new row
nextRow :: (Monad m, MonadState HMapState m) => m ()
nextRow = curSquare %= bimap (const 0) (+ 1)

squareP :: StateParser HMapState ()
squareP = do
  sq <- curSquare <<%= first (+ 1) -- increment current square, returning previous value
  c  <- letterChar
  when (c == 'S') (path . _1 .= Just sq)
  when (c == 'E') (path . _2 .= Just sq)
  when (c == 'a') (lows %= (sq :))
  curGrid %= ((sq, getHeight c) :)
  where
    getHeight c = case c of
      'S' -> 0
      'E' -> 25
      _   -> ord c - 97