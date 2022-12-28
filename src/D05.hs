module D05 ( d05 ) where

import Utils ( prepAnswers2, safeHead, optSpaceP, Answers, Parser )

import Control.Monad ( foldM )
import Data.Char ( isUpper, isAlpha )
import Text.Megaparsec ( satisfy, eof )
import Text.Megaparsec.Char ( char, eol, hspace, newline, space, string )
import qualified Data.IntMap as M
import qualified Text.Megaparsec.Char.Lexer as L

type StackId     = Int
type CrateStacks = IntMap String
type Target      = String
type Payload     = String
type MoveMode    = Payload -> Target -> Target

data CraneModel  = CM9000 | CM9001 deriving Eq
data CrateMove   = CrateMove
  { getQuantity :: Int
  , getSourceId :: StackId
  , getTargetId :: StackId
  }
data CranePlan = CranePlan CrateStacks [CrateMove]

-- *** SOLUTION *** --
d05 :: FilePath -> Text -> Answers
d05 = prepAnswers2 (moveAll CM9000) (moveAll CM9001) cranePlanP

moveAll :: CraneModel -> CranePlan -> String
moveAll crane (CranePlan initialStacks ms) = fromMaybe "Error: invalid input" $ do
  finalStacks <- foldM (if crane == CM9000 then moveLoad m9000 else moveLoad m9001) initialStacks ms
  traverse safeHead $ toList finalStacks
  where m9000, m9001 :: MoveMode
        m9000 = flip (foldl' (flip (:))) -- transfer crates one at a time (reverse payload order)
        m9001 = flip (foldr (:)) -- transfer crates all together (payload order)

moveLoad :: MoveMode -> CrateStacks -> CrateMove -> Maybe CrateStacks
moveLoad mm stacks (CrateMove q sId tId) = do
  src <- M.lookup sId stacks
  let payload = take q src
      stacks' = M.adjust (drop q) sId stacks
  pure $ M.adjust (mm payload) tId stacks'

-- *** PARSERS *** --
cranePlanP :: Parser CranePlan
cranePlanP = do
  cs <- some cratesP
  ms <- labelP <* newline *> some moveP <* eof
  -- transpose grid, remove leading spaces and convert to column Map
  let cmap = M.fromList . zip [1 :: Int ..] . map (filter isAlpha) $ transpose cs
  pure $ CranePlan cmap ms

cratesP :: Parser String
cratesP = some (crateP <|> emptyP) <* eol

crateP :: Parser Char
crateP = do
  c <- char '[' *> satisfy isUpper <* char ']'
  optSpaceP >> pure c

emptyP :: Parser Char
emptyP = string "   " >> optSpaceP >> pure ' '

labelP :: Parser [Int]
labelP = many (hspace *> L.decimal <* hspace) <* newline

moveP :: Parser CrateMove
moveP = do
  q <- string "move" <* space *> L.decimal
  sId <- space <* string "from" <* space *> L.decimal
  tId <- space <* string "to" <* space *> L.decimal <* newline
  pure $ CrateMove { getQuantity = q, getSourceId = sId, getTargetId = tId }