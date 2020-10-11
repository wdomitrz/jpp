module Lab06parsingStateTParser2(Parser,runParser,item) where
import Control.Monad.State
import Lab05transformers (MaybeT(..))
import Control.Monad.Identity

-- Use the StateT transformer on MaybeT on Identity
type Parser a = StateT String (MaybeT Identity) a

runParser :: Parser a -> String -> Maybe (a,String)
runParser m s = runIdentity $ runMaybeT $ runStateT m s

item :: Parser Char
item = do
    xs <- get
    case xs of
      [] -> fail ""
      (y:ys) -> put ys >> return y
