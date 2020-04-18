module LHEparsec where
import           Text.Parsec
import           Text.Parsec.String
import           Data.Functor                   ( void )
import           Control.Monad                  ( forM_ )
import           Control.Monad                  ( mfilter )

-- 1.
-- c
skipSpaces :: Parser ()
skipSpaces = void $ many $ oneOf " \t\f\r\n"

skipSpaces1 :: Parser ()
skipSpaces1 = void $ many1 $ oneOf " \t\f\r\n"

skipString :: String -> Parser ()
skipString s = void $ skipSpaces *> string s

skipString1 :: String -> Parser ()
skipString1 s = void $ skipSpaces *> string s <* skipSpaces

data Exp
  = EInt Int             -- stała całkowita
  | EAdd Exp Exp         -- e1 + e2
  | ESub Exp Exp         -- e1 - e2
  | EMul Exp Exp         -- e1 * e2
  | EVar String          -- zmienna
  | ELet String Exp Exp  -- let var = e1 in e2
    deriving (Show, Eq, Ord)

expression :: Parser Exp
expression = exp0 <* skipSpaces

expInt :: Parser Exp
expInt = EInt . read <$> (skipSpaces *> many1 digit)

parVar :: Parser String
parVar =
    skipSpaces
        *> (mfilter (/= "let") ((:) <$> letter <*> many (try letter <|> digit)))

expVar :: Parser Exp
expVar = EVar <$> parVar

expAdd :: Parser Exp
expAdd = skipString "+" *> (EAdd <$> exp1) <*> exp2

expSub :: Parser Exp
expSub = skipString "-" *> (ESub <$> exp1) <*> exp2

expMul :: Parser Exp
expMul = skipString "*" *> (EMul <$> exp2) <*> exp3

exp0, exp1, exp2, exp3 :: Parser Exp
exp0 = try expLet <|> exp1
exp1 = try expAdd <|> (try expSub <|> exp2)
exp2 = try expMul <|> exp3
exp3 =
    try (skipString "(" *> exp0 <* skipString ")") <|> (try expVar <|> expInt)

expLet :: Parser Exp
expLet =
    ELet
        <$> (skipString "let" *> skipSpaces1 *> parVar)
        <*> (skipString1 "=" *> exp1)
        <*> (skipString1 "in" *> exp0)

run :: Parser a -> String -> Either ParseError a
run p = parse p "<interactive>"

-- | expression
-- >>> run expression "let x = 7 in let y = (+ 3 4) in + * 2 x y"
-- Right (ELet "x" (EInt 7) (ELet "y" (EAdd (EInt 3) (EInt 4)) (EAdd (EMul (EInt 2) (EVar "x")) (EVar "y"))))
-- >>> run expression "* (+ x y) z"
-- Right (EMul (EAdd (EVar "x") (EVar "y")) (EVar "z"))
-- >>> run expression "+ * x y z"
-- Right (EAdd (EMul (EVar "x") (EVar "y")) (EVar "z"))
-- >>> run expression " ( (  \n   ( \t \n 42 )   ) )\n"
-- Right (EInt 42)
