import           Data.Char
import           Data.Maybe                     ( isNothing
                                                , fromJust
                                                , mapMaybe
                                                )
import           Prelude                 hiding ( Either(..) )

-- 1.
foldMaybe :: c -> (a -> c) -> Maybe a -> c
foldMaybe x _ Nothing  = x
foldMaybe _ f (Just y) = f y
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just x) = x
maybeHead :: [a] -> Maybe a
maybeHead []      = Nothing
maybeHead (x : _) = Just x
foldEither :: (a -> c) -> (b -> c) -> Either a b -> c
foldEither f _ (Left  x) = f x
foldEither _ f (Right x) = f x
mapEither :: (a1 -> a2) -> (b1 -> b2) -> Either a1 b1 -> Either a2 b2
mapEither f _ (Left  x) = Left $ f x
mapEither _ f (Right x) = Right $ f x
mapRight :: (b1 -> b2) -> Either a b1 -> Either a b2
mapRight - (Left x) = Left x
mapRight f (Right x) = Right $ f x
fromEither :: Either a a -> a
fromEither (Left  x) = x
fromEither (Right x) = x

reverseRight :: Either e [a] -> Either e [a]
reverseRight = fmap reverse

maybeHead' :: [a] -> Maybe a
maybeHead' = foldr (const . Just) Nothing

-- 2.
readInts :: String -> [Int]
readInts = mapMaybe readInt . words
  where
    readInt :: String -> Maybe Int
    readInt s | all isDigit s = Just $ read s
              | otherwise     = Nothing
--TODO

-- 3.
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Ord)

-- a.
instance Show a => Show (Tree a) where
    show Empty = ""
    show (Node x t1 t2) =
        "(" ++ show t1 ++ ") " ++ show x ++ " (" ++ show t2 ++ ")"

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
contains x Empty = False
contains x (Node y t1 t2) | x == y    = True
                          | x < y     = contains x t1
                          | otherwise = contains x t2

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insert Empty

-- d.
sort :: (Ord a) => [a] -> [a]
sort = toList . fromList

main :: IO ()
main = interact $ show . sort . readInts

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

instance Show Exp where
    show (EInt x    ) = "EInt" ++ show x
    show (EAdd x y  ) = "EAdd" ++ show x ++ ", " ++ show y
    show (ESub x y  ) = "ESub" ++ show x ++ ", " ++ show y
    show (EMul x y  ) = "EMul" ++ show x ++ ", " ++ show y
    show (EVar s    ) = "EVar" ++ show s
    show (ELet s x y) = "ELet" ++ show s ++ ", " ++ show x ++ ", " ++ show y

-- b.
