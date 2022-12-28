module D10 ( d10 ) where

import Utils ( StateParser, Answers, prepAnswersS )
import Text.Megaparsec ( choice, many )
import Text.Megaparsec.Char ( hspace, string, newline, space )
import Prelude hiding ( many )
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Lens ( use, (%=), makeLenses, (<<%=) )

data PState = PState { _cyc :: Int, _xReg :: Int, _strength :: Int, _screen :: String }
makeLenses ''PState

d10 :: FilePath -> Text -> Answers
d10 = prepAnswersS id progP $ PState 1 1 0 ""

progP :: StateParser PState Answers
progP = do
  _ <- many instP
  str <- use strength
  scn <- ('\n' :) . reverse <$> use screen
  pure (show str, scn)

instP :: StateParser PState ()
instP = choice [ Nothing <$ string "noop"
               , string "addx" *> hspace *> (Just <$> L.signed space L.decimal)
               ] <* newline >>= maybe (loop 1 0) (loop 2)
  where
    loop :: MonadState PState m => Int -> Int -> m ()
    loop delay x | delay < 1 = when (x /= 0) $ xReg %= (+ x)
    loop delay x = do
      c  <- cyc <<%= (+ 1) -- inc. cycle counter, returning old value
      xr <- use xReg
      when (c `elem` [20, 60 .. 220]) $ strength %= (+ xr * c)
      let c' = c `mod` 40
          p  = if (c' - 1) `elem` [xr - 1 .. xr + 1] then '#' else '.'
      screen %= if c' == 0 then ('\n' :) . (p :) else (p :)
      loop (delay - 1) x