import           Control.Arrow                  ( first )
import           Data.Char

-- 1.
head' :: [a] -> a
head' (x : xs) = x
head' l        = head l

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
drop' n (x : xs)    = drop' (n - 1) xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x : xs) | f x       = x : ys
                   | otherwise = ys
    where ys = filter' f xs

map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x : xs) = f x : map' f xs

-- 2.
inits :: [a] -> [[a]]
inits []       = [[]]
inits (x : xs) = [] : map (x :) (inits xs)

-- 3.
partitions :: [a] -> [([a], [a])]
partitions []       = [([], [])]
partitions (x : xs) = ([], x : xs) : map (first (x :)) (partitions xs)

-- 4.
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x : xs) =
    [ insertAt n x ys | n <- [0 .. length xs], ys <- permutations xs ]
  where
    insertAt :: Int -> a -> [a] -> [a]
    insertAt 0 x ys       = x : ys
    insertAt n x (y : ys) = y : insertAt (n - 1) x ys

permutations' :: Eq a => [a] -> [[a]]
permutations' [] = [[]]
permutations' xs = [ x : ys | x <- xs, ys <- permutations' (filter (/= x) xs) ]

permutations'' :: [a] -> [[a]]
permutations'' [] = [[]]
permutations'' xs = go $ partitions xs
  where
    go :: [([a], [a])] -> [[a]]
    go []                  = []
    go ((_ , []    ) : ps) = go ps
    go ((ys, z : zs) : ps) = map (z :) (permutations'' (ys ++ zs)) ++ go ps

-- 5.
nub :: Eq a => [a] -> [a]
nub []       = []
nub (x : xs) = x : nub (filter (x /=) xs)

-- 5.
triples :: Int -> [(Int, Int, Int)]
triples n = [ (x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n] ]

-- 6.
triads :: Int -> [(Int, Int, Int)]
triads n =
    [ (x, y, z)
    | x <- [1 .. n]
    , y <- [1 .. n]
    , x <= y
    , z <- [1 .. n]
    , x ^ 2 + y ^ 2 == z ^ 2
    , gcd x y == 1
    ]

-- 7.
primes :: [Int]
primes = go [2 ..] where go (x : xs) = x : go (filter ((0 /=) . (`mod` x)) xs)

fibs :: Int -> Int
fibs n = go n 1 1
  where
    go 0 x _ = x
    go n x y = go (n - 1) y (x + y)

factorioal :: Int -> Int
factorioal n = go n 1
  where
    go 0 acc = acc
    go n acc = go (n - 1) (n * acc)

reverse' :: [a] -> [a]
reverse' = foldr (:) []
reverse'' :: [a] -> [a]
reverse'' = go []  where
    go :: [a] -> [a] -> [a]
    go ys []       = ys
    go ys (z : zs) = go (z : ys) zs

-- 8.
indexOf :: Char -> String -> Maybe Int
indexOf x = go 0
  where
    go :: Int -> String -> Maybe Int
    go _ [] = Nothing
    go k (y : ys) | x == y    = Just k
                  | otherwise = go (k + 1) ys

positions :: Char -> String -> [Int]
positions x = go 1
  where
    go :: Int -> String -> [Int]
    go _ [] = []
    go n (y : xs) | x == y    = n : rest
                  | otherwise = rest
        where rest = go (n + 1) xs

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = reverse $ go 0 xs []
  where
    go _ []       ps = ps
    go n (y : ys) ps = go (n + 1) ys r where r = if x == y then n : ps else ps

positions'' :: Eq a => a -> [a] -> [Int]
positions'' x = reverse . snd . foldl
    (\(n, ps) y -> (n + 1, if x == y then n : ps else ps))
    (1, [])

-- 9.
showInt :: Int -> String
showInt 0         = "0"
showInt n | n < 0 = '-' : showInt (-n)
showInt n         = reverse $ go n
  where
    go :: Int -> String
    go 0         = ""
    go n | n < 0 = '-' : go (-n)
    go n         = chr (ord '0' + n `mod` 10) : go (n `div` 10)

showIntLst :: [Int] -> String
showIntLst []       = ""
showIntLst [x     ] = showInt x
showIntLst (x : xs) = showInt x ++ ", " ++ showIntLst xs

showLst :: (a -> String) -> [a] -> String
showLst _ []       = ""
showLst f [x     ] = f x
showLst f (x : xs) = f x ++ ", " ++ showLst f xs

-- 10.
divide :: Int -> String -> String
divide n = unlines . concatMap (\v -> if null v then [[]] else go n v) . lines
  where
    go :: Int -> [a] -> [[a]]
    go n [] = []
    go n l  = take n l : go n (drop n l)

main :: IO ()
main = interact $ divide 3

-- 11.
-- a.
incAll :: [[Int]] -> [[Int]]
incAll = map $ map (1 +)

-- b.
factorial' :: Int -> Int
factorial' n = foldr (*) 1 [1 .. n]

concat' :: [[a]] -> [a]
concat' = foldr (++) []
concat'' :: [[a]] -> [a]
concat'' = foldl (++) []

-- test using
-- :set +s
-- :unset +s

-- c.
-- ad 5.
asd :: [a] -> [a]
asd (x:xs) = []
