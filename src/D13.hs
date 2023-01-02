module D13 ( d13 ) where

import Utils ( prepAnswersS, Answers, StateParser )

import Control.Lens ( (^.), (<<%=), (%=), makeLenses )
import Data.Traversable ( for )
import Prelude hiding ( some, many )
import Relude.Extra ( dup )
import Text.Megaparsec ( choice, count, many, some, eof )
import Text.Megaparsec.Char ( char, newline )
import qualified Data.Map as M
import qualified Text.Megaparsec.Char.Lexer as L

data PVal = I Int | L [PVal] deriving Eq
instance Ord PVal where
  compare   (I l )     (I r )     = compare l r -- compare using Int instance
  compare l@(L _ )   r@(I _ )     = compare l (L [r]) -- convert Int values to list
  compare l@(I _ )   r@(L _ )     = compare (L [l]) r
  compare   (L [])     (L [])     = EQ -- continue if lists have same length
  compare   (L [])     (L _ )     = LT -- in order if left list is shorter
  compare   (L _ )     (L [])     = GT -- out of order if right list is shorter
  compare   (L (x:xs)) (L (y:ys)) = case compare x y of
    EQ -> compare (L xs) (L ys) -- continue comparing tails if heads compare EQ
    o  -> o

data PState  = PS { _curIdx :: Int, _curSum :: Int, _packets :: [PVal] }
makeLenses ''PState

d13 :: FilePath -> Text -> Answers
d13 = prepAnswersS (bimap show (fromMaybe "Missing divider packet(s)!" . d13_2)) sigP $ PS 1 0 sps
  where
    sps   = [L [L [I 2]], L [L [I 6]]] -- spacer packets: [[2]],[[6]]
    d13_2 = fmap (show . product @Int) . for sps . flip M.lookup . M.fromList . (`zip` [1..]) . sort

sigP :: StateParser PState (Int, [PVal])
sigP = bimap (^. curSum) (^. packets) . dup <$> (some packetsP *> get)

packetsP :: StateParser PState ()
packetsP = do
  i <- curIdx <<%= (+ 1)
  ps@[p1, p2] <- count 2 (listP <* newline) <* (void newline <|> eof)
  packets %= (ps ++)
  when (p1 <= p2) (curSum %= (+ i)) -- if packets in order, inc sum by their idx
  where
    listP = char '[' *> fmap L (many (choice [intP, listP])) <* char ']' <* optional (char ',')
    intP  = I <$> L.decimal <* optional (char ',')