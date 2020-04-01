{-# LANGUAGE FlexibleContexts #-}
module LM2 where -- required by doctest
import qualified Data.Map                      as Map
import           Data.Map                       ( (!)
                                                , empty
                                                , Map
                                                , insert
                                                )
import           Control.Monad.Reader
import           Control.Monad.State     hiding ( State )
import           LH2                            ( fromList
                                                , toList
                                                , Tree(Empty, Node)
                                                )
import           LH1                            ( fibs )
-- 1.
{- | allPairs
>>> allPairs [1,2,3] [4,5]
[[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]
>>> allPairs' [1,2,3] [4,5]
[[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]
-}
allPairs :: [a] -> [a] -> [[a]]
allPairs xs ys = xs >>= (`fmap` ys) . (. return) . (:)

allPairs' :: [a] -> [a] -> [[a]]
allPairs' xs ys = do
    x <- xs
    y <- ys
    return [x, y]

{- | allCombinations
>>> allCombinations [[1,2], [4,5], [6], [7]]
[[1,4,6,7],[1,5,6,7],[2,4,6,7],[2,5,6,7]]
>>> allCombinations' [[1,2], [4,5], [6], [7]]
[[1,4,6,7],[1,5,6,7],[2,4,6,7],[2,5,6,7]]
-}
allCombinations :: [[a]] -> [[a]]
allCombinations []         = [[]]
allCombinations (xs : xss) = xs >>= (<$> allCombinations xss) . (:)

allCombinations' :: [[a]] -> [[a]]
allCombinations' []         = [[]]
allCombinations' (xs : xss) = do
    x  <- xs
    ys <- allCombinations' xss
    return (x : ys)

-- 2.
-- a.
{- | renumber
>>> renumber (Node 'a' (Node 'a' (Node 'b' Empty Empty) (Node 'c' Empty Empty)) (Node 'c' Empty Empty))
Node 0 (Node 1 (Node 2 Empty Empty) (Node 2 Empty Empty)) (Node 1 Empty Empty)
>>> renumberReader (Node 'a' (Node 'a' (Node 'b' Empty Empty) (Node 'c' Empty Empty)) (Node 'c' Empty Empty))
Node 0 (Node 1 (Node 2 Empty Empty) (Node 2 Empty Empty)) (Node 1 Empty Empty)
>>> renumberReader' (Node 'a' (Node 'a' (Node 'b' Empty Empty) (Node 'c' Empty Empty)) (Node 'c' Empty Empty))
Node 0 (Node 1 (Node 2 Empty Empty) (Node 2 Empty Empty)) (Node 1 Empty Empty)
>>> renumberFun (Node 'a' (Node 'a' (Node 'b' Empty Empty) (Node 'c' Empty Empty)) (Node 'c' Empty Empty))
Node 0 (Node 1 (Node 2 Empty Empty) (Node 2 Empty Empty)) (Node 1 Empty Empty)
>>> renumberFun' (Node 'a' (Node 'a' (Node 'b' Empty Empty) (Node 'c' Empty Empty)) (Node 'c' Empty Empty))
Node 0 (Node 1 (Node 2 Empty Empty) (Node 2 Empty Empty)) (Node 1 Empty Empty)
-}

renumber :: Tree a -> Tree Int
renumber = go 0
  where
    go :: Int -> Tree a -> Tree Int
    go _ Empty        = Empty
    go n (Node _ l r) = Node n (go (n + 1) l) (go (n + 1) r)

-- renumber for MonadReader
renumberM :: MonadReader Int m => Tree a -> m (Tree Int)
renumberM Empty = return Empty
renumberM (Node _ l r) =
    asks Node <*> local (+ 1) (renumberM l) <*> local (+ 1) (renumberM r)

-- renumber for MonadReader
renumberM' :: MonadReader Int m => Tree a -> m (Tree Int)
renumberM' Empty        = return Empty
renumberM' (Node _ l r) = do
    l' <- local (+ 1) (renumberM' l)
    r' <- local (+ 1) (renumberM' r)
    n  <- ask
    return (Node n l' r')

-- Reader a as MonadReader
renumberReader :: Tree a -> Tree Int
renumberReader =
    flip runReader 0 . (renumberM :: Tree a -> Reader Int (Tree Int))

-- Reader a as MonadReader
renumberReader' :: Tree a -> Tree Int
renumberReader' =
    flip runReader 0 . (renumberM' :: Tree a -> Reader Int (Tree Int))

-- (->) a as MonadReader
renumberFun :: Tree a -> Tree Int
renumberFun = flip ($) 0 . (renumberM :: Tree a -> Int -> Tree Int)

-- (->) a as MonadReader
renumberFun' :: Tree a -> Tree Int
renumberFun' = flip ($) 0 . (renumberM' :: Tree a -> Int -> Tree Int)


-- b.
type Var = String
data Exp = EInt Int
    | EOp  Op Exp Exp
    | EVar Var
    | ELet Var Exp Exp  -- let var = e1 in e2
    deriving Show

data Op = OpAdd | OpMul | OpSub deriving Show
type State = Map Var Int
{- | evalExp
>>> let test = ELet "x" (ELet "y" (EOp OpAdd (EInt 6) (EInt 9)) (EOp OpSub y (EInt 1))) (EOp OpMul x (EInt 3)) where x = EVar "x"; y = EVar "y"
>>> evalExp test
42
>>> evalExp' test
42
-}
evalExp :: Exp -> Int
evalExp = flip ($) empty . evalExpM
evalExp' :: Exp -> Int
evalExp' = flip runReader empty . evalExpM

evalExpM :: MonadReader State m => Exp -> m Int
evalExpM (EInt n     ) = return n
evalExpM (EOp o e1 e2) = op <$> evalExpM e1 <*> evalExpM e2
  where
    op :: Int -> Int -> Int
    op = case o of
        OpAdd -> (+)
        OpMul -> (*)
        OpSub -> (-)
evalExpM (EVar v      ) = asks (! v)
evalExpM (ELet v e1 e2) = evalExpM e1 >>= flip local (evalExpM e2) . insert v

-- 3.
-- a.
renumberTreeM :: MonadState Int m => Tree a -> m (Tree Int)
renumberTreeM Empty        = return Empty
renumberTreeM (Node _ l r) = do
    l' <- renumberTreeM l
    n  <- get
    modify (+ 1)
    r' <- renumberTreeM r
    return (Node n l' r')

{- | renumberTree
>>> (toList $ renumberTree $ fromList "Learn Haskell") == [0..12]
True
-}
renumberTree :: Tree a -> Tree Int
renumberTree = flip evalState 0 . renumberTreeM

-- b.
data BExp = BTrue
          | BFalse
          | BNot BExp
          | BOp BOp BExp BExp
          | BCmp Cmp Exp Exp
          deriving Show
data BOp = BAnd | BOr deriving Show
data Cmp = Eq | Lt | Leq | Gt | Geq | Neq deriving Show
evalBExp :: BExp -> Bool
evalBExp = flip ($) empty . evalBExpM

evalBExpM :: MonadReader State m => BExp -> m Bool
evalBExpM BTrue         = return True
evalBExpM BFalse        = return False
evalBExpM (BNot b     ) = not <$> evalBExpM b
evalBExpM (BOp o e1 e2) = do
    b1 <- evalBExpM e1
    b2 <- evalBExpM e2
    return $ op b1 b2
  where
    op :: Bool -> Bool -> Bool
    op = case o of
        BAnd -> (&&)
        BOr  -> (||)
evalBExpM (BCmp c e1 e2) = do
    n1 <- evalExpM e1
    n2 <- evalExpM e2
    return $ cmp n1 n2
  where
    cmp :: Int -> Int -> Bool
    cmp = case c of
        Eq  -> (==)
        Lt  -> (<)
        Leq -> (<=)
        Gt  -> (>)
        Geq -> (>=)
        Neq -> (/=)

data Stmt = SSkip -- skip
          | SAssign Var Exp -- x := e
          | SNext Stmt Stmt -- S1 ; S2
          | SIf BExp Stmt Stmt -- if b then S1 else S2
          | SWhile BExp Stmt -- while b do S
          deriving Show

execStmt :: Stmt -> IO ()
execStmt = print . evalStmt

evalStmt :: Stmt -> State
evalStmt = flip evalState empty . evalStmtM

evalStmtM :: MonadState State m => Stmt -> m State
evalStmtM SSkip         = get
evalStmtM (SAssign v e) = do
    s <- get
    let n = evalExpM e s
    modify (insert v n)
    get
evalStmtM (SNext s1 s2) = do
    evalStmtM s1
    evalStmtM s2
evalStmtM (SIf b i1 i2) = do
    s <- get
    let b' = evalBExpM b s
    evalStmtM (if b' then i1 else i2)
evalStmtM w@(SWhile b i) = evalStmtM (SIf b (SNext i w) SSkip)


-- c.

data Decl = Decl Var Exp

data Stmt' = SSkip' -- skip
           | SAssign' Var Exp -- x := e
           | SNext' Stmt' Stmt' -- S1 ; S2
           | SIf' BExp Stmt' Stmt' -- if b then S1 else S2
           | SWhile' BExp Stmt' -- while b do S
           | SDecl [Decl] Stmt' -- begin [D] S end

type Loc = Int

type Store = Map Loc Int
type VEnv = Map Var Loc

alloc :: Store -> Loc
alloc s = if null s then 0 else (1 +) $ fst $ Map.findMax s

evalStmt' :: Stmt' -> Store
evalStmt' = flip evalState Map.empty . flip evalStmt'M Map.empty

getState :: VEnv -> Store -> State
getState rho s = Map.fromSet go (Map.keysSet rho)
  where
    go :: (Var -> Int)
    go v = s ! (rho ! v)
(+++) :: VEnv -> Store -> State
(+++) = getState

evalDeclM :: MonadReader (VEnv, Store) m => Decl -> m (VEnv, Store)
evalDeclM (Decl v e) = do
    (rho, s) <- ask
    let l = alloc s
    return (insert v l rho, insert l (evalExpM e (rho +++ s)) s)

evalStmt'M :: MonadState Store m => Stmt' -> VEnv -> m Store
evalStmt'M SSkip'         rho = get
evalStmt'M (SAssign' v e) rho = do
    s <- get
    let n = evalExpM e (rho +++ s)
    put (insert (rho ! v) n s)
    get
evalStmt'M (SNext' i1 i2) rho = do
    evalStmt'M i1 rho
    evalStmt'M i2 rho
evalStmt'M (SIf' b i1 i2) rho = do
    s <- get
    let b' = evalBExpM b (rho +++ s)
    evalStmt'M (if b' then i1 else i2) rho
evalStmt'M w@(SWhile' b  i) rho = evalStmt'M (SIf' b (SNext' i w) SSkip') rho
evalStmt'M (  SDecl   ds i) rho = do
    s <- get
    let (rho', s') = foldl (flip evalDeclM) (rho, s) ds
    put s'
    evalStmt'M i rho'


-- Bigger tests
-- 3
-- b

-- This test is based on https://www.mimuw.edu.pl/~klin/teaching/sem19-20/Tiny.hs
{-- Calculate the n+1'th Fibonacci number
x := 0; y := 1; i := 0;
while i<=n do
  tmp := x+y;
  x := y;
  y := tmp;
  i := i+1;
--}
fibStmt :: Stmt
fibStmt = SNext
    (SAssign "x" (EInt 0))
    (SNext
        (SAssign "y" (EInt 1))
        (SNext
            (SAssign "i" (EInt 0))
            (SWhile
                (BCmp Leq (EVar "i") (EVar "n"))
                (SNext
                    (SAssign "tmp" (EOp OpAdd (EVar "x") (EVar "y")))
                    (SNext
                        (SAssign "x" (EVar "y"))
                        (SNext (SAssign "y" (EVar "tmp"))
                               (SAssign "i" (EOp OpAdd (EVar "i") (EInt 1)))
                        )
                    )
                )
            )
        )
    )
fib :: Int -> Int
fib n = (flip evalState (Map.singleton "n" n) . evalStmtM) fibStmt ! "x"
{- | fib
>>> map fib [1 .. 30] == map fibs [1 .. 30]
True
-}

-- | Tests created by Mr. Artur Zaroda <zaroda@mimuw.edu.pl>
-- >>> execStmt test1
-- fromList [("i",0),("s",2432902008176640000)]
-- >>> execStmt test2
-- fromList [("a",37),("b",0),("p",0)]

-- i = 20;
-- s = 1;
-- while (i != 0) {
--     s = s * i;
--     i = i - 1;
-- }
--
test1 :: Stmt
test1 = SNext
    (SAssign "i" $ EInt 20)
    (SNext
        (SAssign "s" $ EInt 1)
        (SWhile
            (BCmp Neq i $ EInt 0)
            (SNext (SAssign "s" $ EOp OpMul s i)
                   (SAssign "i" $ EOp OpSub i (EInt 1))
            )
        )
    )
  where
    s = EVar "s"
    i = EVar "i"

-- a = 2368;
-- b = 3145;
-- while (b) {
--     p = a - b;
--     if (p) {
--         a = p;
--     } else {
--         b = 0 - p;
--     }
-- }
--
test2 :: Stmt
test2 = SNext
    (SAssign "a" $ EInt 2368)
    (SNext
        (SAssign "b" $ EInt 3145)
        (SWhile
            (BCmp Neq (EVar "b") $ EInt 0)
            (SNext
                (SAssign "p" $ EOp OpSub (EVar "a") (EVar "b"))
                (SIf (BCmp Gt (EVar "p") $ EInt 0)
                     (SAssign "a" $ EVar "p")
                     (SAssign "b" $ EOp OpSub (EInt 0) (EVar "p"))
                )
            )
        )
    )


-- c

-- This test is based on https://www.mimuw.edu.pl/~klin/teaching/sem19-20/Declarations.hs
-- EXAMPLE: BASIC SCOPING --
{--
begin
  var x = 123
  var y = 456
  begin
    var x = 789
    x := 7
    y := x+1
  end;
  x := y+1
end
--}
fooStmt :: Stmt'
fooStmt = SDecl
    [Decl "x" (EInt 123), Decl "y" (EInt 456)]
    (SNext'
        (SDecl
            [Decl "x" (EInt 789)]
            (SNext' (SAssign' "x" (EInt 7))
                    (SAssign' "y" (EOp OpAdd (EVar "x") (EInt 1)))
            )
        )
        (SAssign' "x" (EOp OpAdd (EVar "y") (EInt 1)))
    )
{- | decl
>>> evalStmt' fooStmt
fromList [(0,9),(1,8),(2,7)]
-}
