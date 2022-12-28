{-# LANGUAGE GADTs #-}
module D11 ( d11 ) where

import Utils ( prepAnswers2, Answer, Answers, Parser )

import Control.Lens ( (|>), (^.), (%~), (.~), makeLenses )
import Data.IntMap as M ( adjust, elems, foldr, fromList, keys, lookup, size )
import Data.Sequence ( Seq((:<|)) )
import Prelude hiding ( many, last )
import Text.Megaparsec ( choice, many, MonadParsec(eof) )
import Text.Megaparsec.Char ( char, hspace, newline, string )
import qualified Data.Sequence as S
import qualified Text.Megaparsec.Char.Lexer as L

data X      = Var | Val Int
data Monkey = Monkey {
    _mItems     :: Seq Int
  , _mOp        :: Int -> Int
  , _mDivisor   :: Int
  , _mTest      :: Int -> Int
  , _mInspected :: Int }
makeLenses ''Monkey

type MonkeyId  = Int
type MonkeyMap = IntMap Monkey
type Rounds    = Int
type Reliever  = Int -> Int

-- *** SOLUTION *** --
d11 :: FilePath -> Text -> Answers
d11 = prepAnswers2 d11_1 d11_2 monkeysP
  where
    d11_1    = solve 20 (`div` 3)
    d11_2 mm = let lc = M.foldr (\m acc -> lcm acc $ m ^. mDivisor) 1 mm -- calc. lcm from divisors
               in solve 10000 (`mod` lc) mm

solve :: Rounds -> Reliever -> MonkeyMap -> Answer
solve rnds rel mm = case foldlM (throwItems rel) mm turns of
  Nothing  -> "Error: invalid input"
  Just mm' -> show . product . take 2 . sortBy (flip compare) . map (^. mInspected) $ M.elems mm'
  where turns = take (M.size mm * rnds) . cycle $ M.keys mm

throwItems :: Reliever -> MonkeyMap -> MonkeyId -> Maybe MonkeyMap
throwItems rel mm i = do
  monkey <- M.lookup i mm
  let items = monkey ^. mItems
  -- throw all monkey's items, set items to empty and increment inspected count
  pure $ M.adjust ((mInspected %~ (+ S.length items)) . (mItems .~ S.Empty)) i $ go monkey items mm
  where
    go _ S.Empty mm' = mm'
    go m (it :<| its) mm' = let it' = rel (m ^. mOp $ it)
                                nextMonkey = m ^. mTest $ it'
                            in go m its $ M.adjust (mItems %~ (|> it')) nextMonkey mm'

-- *** PARSER *** --
monkeysP :: Parser MonkeyMap
monkeysP = M.fromList <$> many monkeyP

monkeyP :: Parser (Int, Monkey)
monkeyP = do
  i   <- spacedStr "Monkey" *> L.decimal <* char ':' <* newline
  is  <- spacedStr "Starting items:" *> many (L.decimal <* optional (char ',') <* hspace) <* newline
  f   <- spacedStr "Operation: new = old" *> choice [(+) <$ char '+', (*) <$ char '*'] <* hspace
  x   <- choice [Var <$ string "old", Val <$> L.decimal] <* newline
  d   <- spacedStr "Test: divisible by" *> L.decimal <* newline
  tId <- spacedStr "If true:" *> throwP
  fId <- spacedStr "If false:" *> throwP <* (void newline <|> eof)
  pure (i, Monkey { _mItems     = S.fromList is
                  , _mOp        = makeOp f x
                  , _mDivisor   = d
                  , _mTest      = \n -> if n `mod` d == 0 then tId else fId
                  , _mInspected = 0
                  })
  where
    spacedStr str = hspace *> string str *> hspace
    throwP = spacedStr "throw to monkey" *> L.decimal <* newline
    makeOp f  Var    = \x -> f x x
    makeOp f (Val x) = f x