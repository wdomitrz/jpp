module Tests where -- required by doctest
import           Control.Arrow                  ( first )
import           Data.Char

-- 1.
head' :: [a] -> a
head' (x : _) = x
head' l       = head l

tail' :: [a] -> [a]
tail' []       = undefined
tail' (_ : xs) = xs

(+++) :: [a] -> [a] -> [a]
(+++) xs ys = foldr (:) ys xs

take' :: Int -> [a] -> [a]
take' _ []         = []
take' n _ | n <= 0 = []
take' n (x : xs)   = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' _ []          = []
drop' n xs | n <= 0 = xs
drop' n (_ : xs)    = drop' (n - 1) xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs) | f x       = x : ys
                   | otherwise = ys
    where ys = filter' f xs

map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x : xs) = f x : map' f xs

-- 2.
-- | inits
-- >>> inits [1,2]
-- [[],[1],[1,2]]
inits :: [a] -> [[a]]
inits []       = [[]]
inits (x : xs) = [] : map (x :) (inits xs)

-- 3.
-- | partitions
-- >>> partitions [1,2,3]
-- [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])]
partitions :: [a] -> [([a], [a])]
partitions []       = [([], [])]
partitions (x : xs) = ([], x : xs) : map (first (x :)) (partitions xs)

-- 4.
-- | permutations
-- >>> permutations [1,2,3]
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
-- >>> permutations' [1,2,3]
-- [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
permutations :: Eq a => [a] -> [[a]]
permutations [] = [[]]
permutations xs = [ x : ys | x <- xs, ys <- permutations (filter (/= x) xs) ]

permutations' :: [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = go $ partitions xs
  where
    go :: [([a], [a])] -> [[a]]
    go []                  = []
    go ((_ , []    ) : ps) = go ps
    go ((ys, z : zs) : ps) = map (z :) (permutations' (ys ++ zs)) ++ go ps

-- 5.
-- | nub
-- >>> nub [1,2,1,3,1,2,1,4]
-- [1,2,3,4]
nub :: Eq a => [a] -> [a]
nub []       = []
nub (x : xs) = x : nub (filter (x /=) xs)
