module Tests where -- required by doctest
import           Control.Monad.Reader
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
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show)

{- | renumber
>>> renumber (Node 'a' (Node 'a' (Node 'b' Empty Empty) (Node 'c' Empty Empty)) (Node 'c' Empty Empty))
Node 0 (Node 1 (Node 2 Empty Empty) (Node 2 Empty Empty)) (Node 1 Empty Empty)
>>> renumber' (Node 'a' (Node 'a' (Node 'b' Empty Empty) (Node 'c' Empty Empty)) (Node 'c' Empty Empty))
Node 0 (Node 1 (Node 2 Empty Empty) (Node 2 Empty Empty)) (Node 1 Empty Empty)
>>> renumber'' (Node 'a' (Node 'a' (Node 'b' Empty Empty) (Node 'c' Empty Empty)) (Node 'c' Empty Empty))
Node 0 (Node 1 (Node 2 Empty Empty) (Node 2 Empty Empty)) (Node 1 Empty Empty)
-}

renumber :: Tree a -> Tree Int
renumber = go 0
  where
    go :: Int -> Tree a -> Tree Int
    go _ Empty        = Empty
    go n (Node _ l r) = Node n (go (n + 1) l) (go (n + 1) r)

renumber' :: Tree a -> Tree Int
renumber' = flip runReader 0 . go
  where
    go :: Tree a -> Reader Int (Tree Int)
    go Empty        = return Empty
    go (Node _ l r) = asks Node <*> local (+ 1) (go l) <*> local (+ 1) (go r)

renumber'' :: Tree a -> Tree Int
renumber'' = flip runReader 0 . go
  where
    go :: Tree a -> Reader Int (Tree Int)
    go Empty        = return Empty
    go (Node _ l r) = do
        n  <- ask
        l' <- local (+ 1) (go l)
        r' <- local (+ 1) (go r)
        return (Node n l' r')
