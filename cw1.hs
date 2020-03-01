import           Control.Arrow                  ( first )
import           Data.Char

-- 1.
head1 :: [a] -> a
head1 (x : xs) = x
head1 l        = head l

tail1 :: [a] -> [a]
tail1 []       = []
tail1 (_ : xs) = xs

(+++) :: [a] -> [a] -> [a]
(+++) xs ys = foldr (:) ys xs

take1 :: Int -> [a] -> [a]
take1 _ []         = []
take1 n _ | n <= 0 = []
take1 n (x : xs)   = x : take1 (n - 1) xs

drop1 :: Int -> [a] -> [a]
drop1 _ []          = []
drop1 n xs | n <= 0 = xs
drop1 n (x : xs)    = drop1 (n - 1) xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 _ [] = []
filter1 f (x : xs) | f x       = x : filter1 f xs
                   | otherwise = filter1 f xs

map1 :: (a -> b) -> [a] -> [b]
map1 _ []       = []
map1 f (x : xs) = f x : map1 f xs

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
    insertAt 0 x ys       = x : ys
    insertAt n x (y : ys) = y : insertAt (n - 1) x ys

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

-- 8.
indexOf :: Char -> String -> Maybe Int
indexOf _ [] = Nothing
indexOf c (x : xs) | c == x    = Just 1
                   | otherwise = fmap (1 +) (indexOf c xs)

positions :: Char -> String -> [Int]
positions _ [] = []
positions c (x : xs) | c == x    = 1 : rest
                     | otherwise = rest
    where rest = map (1 +) $ positions c xs

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
divide n = go' "\n" . concatMap (\v -> if null v then [[]] else go n v) . lines
  where
    go :: Int -> [a] -> [[a]]
    go n [] = []
    go n l  = take n l : go n (drop n l)
    go' :: [a] -> [[a]] -> [a]
    go' _ []       = []
    go' x (y : ys) = y ++ x ++ go' x ys

main :: IO ()
main = interact $ divide 3

-- 11.
-- a.
incAll :: [[Int]] -> [[Int]]
incAll = map (map (1 +))

-- b.
factorial' :: Int -> Int
factorial' n = foldr (*) 1 [1 .. n]

concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- c.
-- ad 5.
