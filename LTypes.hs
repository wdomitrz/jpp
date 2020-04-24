module LTypes where
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Control.Monad.Trans.Except
import           Control.Monad.Reader

infixr 5 :->
data Type = TInt
          | Type :-> Type
  deriving (Eq, Ord)

instance Show Type where
  showsPrec _ TInt = showString "int"
  showsPrec d (t1 :-> t2) =
    showParen (d > 0) $ showsPrec 1 t1 . showString " -> " . shows t2

type Name = String
data Exp = EInt Int -- constant
         | EVar Name -- named variable
         | ELam Name Type Exp -- \x :: t -> e
         | EApp Exp Exp -- e1 e2
         | ELet Name Exp Exp -- let x = e1 in e2
         deriving (Eq, Ord)

instance Show Exp where
  show (EInt x      ) = show x
  show (EVar x      ) = x
  show (ELam x t e  ) = "\\" ++ x ++ " : " ++ show t ++ " -> " ++ show e
  show (EApp e1 e2  ) = show e1 ++ " (" ++ show e2 ++ ")"
  show (ELet x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2

type Env = Map Name Type
type TypeCheckerM a = ExceptT String (Reader Env) a

typeOf :: Exp -> Either String Type
typeOf e = runReader (runExceptT (typeOfM e)) Map.empty

typeOfM :: Exp -> TypeCheckerM Type
typeOfM (EInt _) = return TInt

typeOfM (EVar x) = do
  rho <- ask
  case x `Map.lookup` rho of
    Just t  -> return t
    Nothing -> throwE $ "Variable not in scope: " ++ x

typeOfM (ELam x t e) = do
  tRes <- local (Map.insert x t) $ typeOfM e
  return (t :-> tRes)

typeOfM e@(EApp e1 e2) = do
  t1 <- typeOfM e1
  t2 <- typeOfM e2
  case t1 of
    TInt ->
      throwE
        $  "In expression\n"
        ++ show e
        ++ "\nThe type of "
        ++ show e1
        ++ ": `"
        ++ show t1
        ++ "` is not a function type"
    (t2' :-> t)
      | t2 == t2'
      -> return t
      | otherwise
      -> throwE
        $  "For expression\n"
        ++ show e2
        ++ "\nCouldn't match expected type \n"
        ++ show t2'
        ++ "\nagainst inferred \n"
        ++ show t2

typeOfM (ELet x e1 e2) = do
  t1 <- typeOfM e1
  local (Map.insert x t1) $ typeOfM e2

typeCheck :: Exp -> Type
typeCheck e = case typeOf e of
  Left  s -> error $ "Type error in\n" ++ show e ++ "\n" ++ s
  Right t -> t

-- Tests
-- 1.
-- Przykładowe lambda-termy
type Exp1 = Type -> Exp
type Exp2 = Type -> Exp1
type Exp3 = Type -> Exp2

int :: Type
int = TInt

mkI :: Exp1
mkI a = ELam "x" a $ EVar "x"

mkK :: Exp2
mkK a b = ELam "x" a $ ELam "y" b $ EVar "x"

intK :: Exp
intK = mkK int int

mkS :: Exp3
mkS a b c = ELam "x" a $ ELam "y" b $ ELam "z" c $ EApp
  (EApp (EVar "x") (EVar "z"))
  (EApp (EVar "y") (EVar "z"))

intS :: Exp
intS = mkS (int :-> int :-> int) (int :-> int) int

-- kombinator omega nie typuje sie w prostym rachunku lambda
mkOmega :: Exp1
mkOmega t = ELam "x" t $ EApp (EVar "x") (EVar "x")

intOmega :: Exp
intOmega = mkOmega TInt

{- | Tests 1.
>>> typeOf (EInt 42)
Right int
>>> typeCheck intK
int -> int -> int
>>> typeCheck intS
(int -> int -> int) -> (int -> int) -> int -> int
>>> typeOf intOmega
Left "In expression\nx (x)\nThe type of x: `int` is not a function type"
>>> typeCheck (EApp intS intK)
(int -> int) -> int -> int
>>> typeOf $ EApp (EApp intS intK) intK
Left "For expression\n\\x : int -> \\y : int -> x\nCouldn't match expected type \nint -> int\nagainst inferred \nint -> int -> int"
>>> typeCheck $ ELet "x" (ELam "x" (TInt) (EVar "x")) (EApp (EVar "x") (EInt 42))
int
>>> typeOf $ ELet "x" (ELam "x" (TInt) (EVar "x")) (EApp (EVar "x") (EVar "x"))
Left "For expression\nx\nCouldn't match expected type \nint\nagainst inferred \nint -> int"
-}
