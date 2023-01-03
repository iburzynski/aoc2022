{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module D09 ( d09 ) where

import Utils ( both, prepAnswersS, Answers, StateParser )

import Data.List ( iterate', (!!) )
import Data.Sequence ( Seq(Empty, (:|>)), (|>) )
import Prelude hiding ( many )
import Text.Megaparsec ( choice, many )
import Text.Megaparsec.Char ( char, hspace1, newline )
import qualified Data.Sequence as Sq
import qualified Data.Set as S
import qualified Text.Megaparsec.Char.Lexer as L

type Knot = (Int, Int)

data Direction = U | D | L | R
data RopeState = RS { getRope :: Seq Knot, getTailVisited :: [Knot] }

d09 :: FilePath -> Text -> Answers
d09 = prepAnswersS (both show) movesP (makeRope 2, makeRope 10)
  where makeRope n = RS (Sq.replicate n (0, 0)) [(0, 0)]

moveN :: Direction -> Int -> RopeState -> RopeState
moveN d n rS = iterate' (move1 d) rS !! n

move1 :: Direction -> RopeState -> RopeState
move1 d (RS r@(_ :|> t) vis) = RS r' (if t' /= t then t' : vis else vis)
  where
    r'@(_ :|> t')                 = foldl' moveKnot Empty r
    moveKnot Empty k              = Sq.singleton $ moveHead d k
    moveKnot ks@(_ :|> newHead) k = ks |> moveTail newHead k
move1 _ rS@(RS Empty _)      = rS -- an empty rope doesn't change (not encountered in practice)

moveHead :: Direction -> Knot -> Knot
moveHead d = case d of
  L -> first  (subtract 1)
  R -> first  (+ 1)
  U -> second (+ 1)
  D -> second (subtract 1)

moveTail :: Knot -> Knot -> Knot
moveTail (hx, hy) t@(tx, ty)
  | (distGT1 hx tx && distGT1 hy ty) ||
    (distGT1 hx tx && distEQ1 hy ty) ||
    (distGT1 hy ty && distEQ1 hx tx)    = bimap  (+ signum (hx - tx)) (+ signum (hy - ty)) t
  | distGT1 hx tx                       = first  (+ signum (hx - tx)) t
  | distGT1 hy ty                       = second (+ signum (hy - ty)) t
  | otherwise                           = t
  where
    compareDistWith1 o p1 p2 = compare (abs (p1 - p2)) 1 == o
    distEQ1                  = compareDistWith1 EQ
    distGT1                  = compareDistWith1 GT

-- *** PARSER *** --
movesP :: StateParser (RopeState, RopeState) (Int, Int)
movesP = many moveP >> both (S.size . S.fromList . getTailVisited) <$> get

moveP :: StateParser (RopeState, RopeState) ()
moveP = do
  d <- directionP <* hspace1
  n <- L.decimal  <* newline
  modify (both (moveN d n))

directionP :: StateParser (RopeState, RopeState) Direction
directionP =  choice [U <$ char 'U', D <$ char 'D', L <$ char 'L', R <$ char 'R']