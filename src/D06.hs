module D06 ( d06 ) where

import Utils ( both, Answers )

import Control.Monad ( foldM )
import Data.Text ( unpack )
import qualified Data.Set as S

type MarkerSize = Int

d06 :: FilePath -> Text -> Answers
d06 _ txt = both ($ unpack txt) (getMarkerPos 4, getMarkerPos 14)

getMarkerPos :: MarkerSize -> String -> String
getMarkerPos mSize = either show (const "Marker not found!") . foldM go (0 :: Int, "") where
  go (n, cs) c = let cs' = take mSize (c:cs) in
    if S.size (S.fromList cs') == mSize then Left (n + 1) else Right (n + 1, cs')