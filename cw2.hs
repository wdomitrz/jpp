import           Data.Char                      ( isDigit )
import           Text.Read                      ( readEither )

-- 1.:q

foldMaybe :: c -> (a -> c) -> Maybe a -> c
foldMaybe x _ Nothing  = x
foldMaybe _ f (Just x) = f x
fromMaybe :: a -> Maybe a -> a
fromMaybe = flip foldMaybe id
maybeHead :: [a] -> Maybe a
maybeHead = foldr (const . Just) Nothing
foldEither :: (a -> c) -> (b -> c) -> Either a b -> c
foldEither f _ (Left  x) = f x
foldEither _ g (Right x) = g x
mapEither :: (a1 -> a2) -> (b1 -> b2) -> Either a1 b1 -> Either a2 b2
mapEither = flip $ flip (foldEither . (Left .)) . (Right .)
mapRight :: (b1 -> b2) -> Either a b1 -> Either a b2
mapRight = fmap
fromEither :: Either a a -> a
fromEither = foldEither id id

reverseRight :: Either e [a] -> Either e [a]
reverseRight = fmap reverse
-- 2.
-- a.
readInts :: String -> [Int]
readInts = map read . filter (all isDigit) . words

-- b.
readInts2 :: String -> Either String [Int]
readInts2 = mapM readWithError . words
  where
    readWithError :: String -> Either String Int
    readWithError = go <*> readEither
    go :: String -> Either String Int -> Either String Int
    go = flip mapEither id . (const . ("Not an integer: " ++))

-- c.
sumInts :: String -> String
sumInts = either id (show . sum) . readInts2


main2c :: IO ()
main2c = interact sumInts

-- 3.
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Ord)

-- a.
instance Show a => Show (Tree a) where
    show Empty          = ""
    show (Node x t1 t2) = show x ++ " (" ++ show t1 ++ ") (" ++ show t2 ++ ")"

instance Eq a => Eq (Tree a) where
    Empty        == Empty           = True
    Empty        == _               = False
    _            == Empty           = False
    Node x t1 t2 == Node x' t1' t2' = (x == x') && (t1 == t1') && (t2 == t2')

-- b.
toList :: Tree a -> [a]
toList = go []
  where
    go :: [a] -> Tree a -> [a]
    go l Empty          = l
    go l (Node x t1 t2) = go (x : go l t2) t1

-- c.
insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node x Empty Empty
insert x (Node y t1 t2) | x < y     = Node y (insert x t1) t2
                        | otherwise = Node y t1 $ insert x t2

contains :: (Ord a) => a -> Tree a -> Bool
contains _ Empty = False
contains x (Node y t1 t2) | x == y    = True
                          | x < y     = contains x t1
                          | otherwise = contains x t2

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert Empty

-- d.
sort :: (Ord a) => [a] -> [a]
sort = toList . fromList

main3d :: IO ()
main3d = interact $ show . sort . readInts

-- 4.
data Exp
  = EInt Int             -- stała całkowita
  | EAdd Exp Exp         -- e1 + e2
  | ESub Exp Exp         -- e1 - e2
  | EMul Exp Exp         -- e1 * e2
  | EVar String          -- zmienna
  | ELet String Exp Exp  -- let var = e1 in e2

-- a.
instance Eq Exp where
    (==) (EInt x    ) (EInt x'      ) = x == x'
    (==) (EAdd x y  ) (EAdd x' y'   ) = x == x' && y == y'
    (==) (ESub x y  ) (ESub x' y'   ) = x == x' && y == y'
    (==) (EMul x y  ) (EMul x' y'   ) = x == x' && y == y'
    (==) (EVar s    ) (EVar s'      ) = s == s'
    (==) (ELet s x y) (ELet s' x' y') = s == s' && x == x' && y == y'
    (==) _            _               = False

instance Show Exp where
    show (EInt x  ) = show x
    show (EAdd x y) = show x ++ " + " ++ show y
    show (ESub x y) = show x ++ " - (" ++ show y ++ ")"
    show (EMul x y) = "(" ++ show x ++ ") * (" ++ show y ++ ")"
    show (EVar s  ) = s
    show (ELet s x y) =
        "let " ++ s ++ " = (" ++ show x ++ ") in (" ++ show y ++ ")"

-- b.
instance Num Exp where
    (+)         = EAdd
    (-)         = ESub
    (*)         = EMul
    abs         = EMul =<< signum
    signum      = undefined
    fromInteger = EInt . fromInteger

simpl :: Exp -> Exp
simpl = go' . go
  where
    go :: Exp -> Exp
    go (EAdd x y  ) = EAdd (simpl x) (simpl y)
    go (ESub x y  ) = ESub (simpl x) (simpl y)
    go (EMul x y  ) = EMul (simpl x) (simpl y)
    go (ELet s x y) = simpl $ go'' y
      where
        go'' :: Exp -> Exp
        go'' (EVar z) | s == z    = v
                      | otherwise = EVar z
        go'' (EAdd x' y') = EAdd (go'' x') (go'' y')
        go'' (ESub x' y') = ESub (go'' x') (go'' y')
        go'' (EMul x' y') = EMul (go'' x') (go'' y')
        go'' (ELet z x' y') | s == z    = ELet z x' y'
                            | otherwise = ELet z (go'' x') (go'' y')
        go'' x' = x'
        v :: Exp
        v = simpl x
    go x = x
    go' :: Exp -> Exp
    go' (EAdd (EInt 0) x       ) = x
    go' (EAdd x        (EInt 0)) = x
    go' (ESub x        (EInt 0)) = x
    go' (EMul (EInt 1) x       ) = x
    go' (EMul x        (EInt 1)) = x
    go' (EMul (EInt 0) _       ) = EInt 0
    go' (EMul _        (EInt 0)) = EInt 0
    go' x                        = x

deriv :: String -> Exp -> Exp
deriv s = simpl . go False . simpl
  where
    -- first Bool indicater if variable is shadowed
    go :: Bool -> Exp -> Exp
    go _ (EInt _) = EInt 0
    go b (EVar z) | b || s /= z = EVar (z ++ "'")
                  | otherwise   = EInt 1
    go b (EAdd x y) = EAdd (go b x) (go b y)
    go b (ESub x y) = ESub (go b x) (go b y)
    go b (EMul x y) = EAdd (EMul (go b x) y) (EMul x (go b y))
    go b (ELet z x y) | b || s == z = ELet z x (go True y)
                      | otherwise   = ELet z x (go b y)
-- 5.
-- a.
data Either' a b = Left' a | Right' b

instance Functor (Either' e) where
    fmap _ (Left'  x) = Left' x
    fmap f (Right' x) = Right' (f x)

-- b.
instance Functor Tree where
    fmap _ Empty          = Empty
    fmap f (Node x t1 t2) = Node (f x) (fmap f t1) (fmap f t2)

-- c.
reverseRight' :: Either' e [a] -> Either' e [a]
reverseRight' = fmap reverse

-- d.
class Pointed f where
    pure :: a -> f a

instance Pointed [] where
    pure = return

instance Pointed Maybe where
    pure = return

instance Pointed Tree where
    pure = flip (flip Node Empty) Empty

-- 6.
infixl 4 <**>
class Pointed f => Applicative' f where
      (<**>) :: f(a->b) -> f a -> f b

instance Applicative' Maybe where
    (<**>) Nothing  _        = Nothing
    (<**>) _        Nothing  = Nothing
    (<**>) (Just f) (Just x) = Just $ f x

instance Applicative'  [] where
    (<**>) = flip (flip foldr [] . ((++) .) . flip fmap)
