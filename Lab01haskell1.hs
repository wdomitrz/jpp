module Lab01haskell1 where -- required by doctest
import           Control.Arrow                  ( first )
import           Data.Char
import           Lab00haskell0                  ( inits )

-- 1.
triples :: Int -> [(Int, Int, Int)]
triples n = [ (x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n] ]

-- 2.
-- | triads
-- >>> triads 20
-- [(3,4,5),(5,12,13),(8,15,17)]
triads :: Int -> [(Int, Int, Int)]
triads n =
    [ (x, y, z)
    | (x, y, z) <- triples n
    , x <= y
    , x ^ (2 :: Int) + y ^ (2 :: Int) == z ^ (2 :: Int)
    , gcd x y == 1
    ]

-- 3.
-- | primes
-- >>> take 6 primes
-- [2,3,5,7,11,13]
primes :: [Int]
primes = go [2 ..]  where
    go (x : xs) = x : go (filter ((0 /=) . (`mod` x)) xs)
    go []       = undefined

-- | fibs
-- >>> fibs 7
-- 21
fibs :: Int -> Int
fibs = flip (`go` 1) 1
  where
    go 0 x _ = x
    go n x y = go (n - 1) y (x + y)

-- | factorial
-- >>> factorial 5
-- 120
factorial :: Int -> Int
factorial = flip go 1
  where
    go 0 acc = acc
    go n acc = go (n - 1) (n * acc)

-- | reverse
-- >>> reverse' [1, 2, 3]
-- [3,2,1]
-- >>> reverse'' [1, 2, 3]
-- [3,2,1]
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []
reverse'' :: [a] -> [a]
reverse'' = go []  where
    go :: [a] -> [a] -> [a]
    {- HLINT ignore "Use foldl" -}
    go ys []       = ys
    go ys (z : zs) = go (z : ys) zs

-- 4.
-- | indexOf
-- >>> indexOf 'a' "Ala"
-- Just 2
-- >>> indexOf 'b' "Ala"
-- Nothing
indexOf :: Char -> String -> Maybe Int
indexOf x = go 0
  where
    go :: Int -> String -> Maybe Int
    go _ [] = Nothing
    go k (y : ys) | x == y    = Just k
                  | otherwise = go (k + 1) ys

-- | positions
-- >>> positions 'a' "Ala ma kota"
-- [2,5,10]
-- >>> positions 'b' "Ala ma kota"
-- []
-- >>> positions' 'a' "Ala ma kota"
-- [2,5,10]
-- >>> positions' 'b' "Ala ma kota"
-- []
-- >>> positions'' 'a' "Ala ma kota"
-- [2,5,10]
-- >>> positions'' 'b' "Ala ma kota"
-- []
positions :: Char -> String -> [Int]
positions x = go 0
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
    (0, [])

-- 5.
-- a.
-- | incAll
-- >>> incAll $ inits [1..3]
-- [[],[2],[2,3],[2,3,4]]
incAll :: [[Int]] -> [[Int]]
incAll = map $ map (1 +)

-- b.
-- | factorial'
-- >>> factorial' 5
-- 120
-- >>> factorial'' 5
-- 120
{- HLINT ignore "Use product" -}
factorial' :: Int -> Int
factorial' n = foldr (*) 1 [1 .. n]
factorial'' :: Int -> Int
factorial'' n = product [1 .. n]

-- | concat'
-- >>> concat' [[1,2],[3],[4,5]]
-- [1,2,3,4,5]
{- HLINT ignore "Use concat" -}
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- | maximum'
-- >>> maximum' [1, 2, -3]
-- 2
maximum' :: [Int] -> Int
maximum' []       = error "maximum': empty list"
maximum' (x : xs) = foldl max x xs

-- | minimum'
-- >>> minimum' [-1, 2, -3]
-- -3
minimum' :: [Int] -> Int
minimum' []       = error "minimum': empty list"
minimum' (x : xs) = foldl min x xs

-- | winner
-- >>> winner max [1, 2, -3]
-- 2
winner :: (a -> a -> a) -> [a] -> a
winner _ []       = error "winner: empty list"
winner f (x : xs) = foldl f x xs

-- c.
-- | nub
-- >>> nub [1,2,1,3,1,2,1,4]
-- [1,2,3,4]
nub :: Eq a => [a] -> [a]
nub []       = []
nub (x : xs) = x : nub (filter (x /=) xs)

-- d.
-- | dotProduct
-- >>> dotProduct [1,2,3] [4,5,-6]
-- -4
dotProduct :: [Int] -> [Int] -> Int
dotProduct = (sum .) . zipWith (*)

-- 7.
showInt :: Int -> String
showInt 0         = "0"
showInt n | n < 0 = '-' : showInt (-n)
showInt n         = reverse $ go n
  where
    go :: Int -> String
    go 0          = ""
    go n' | n < 0 = '-' : go (-n')
    go n'         = chr (ord '0' + n' `mod` 10) : go (n' `div` 10)

showIntLst :: [Int] -> String
showIntLst []       = ""
showIntLst [x     ] = showInt x
showIntLst (x : xs) = showInt x ++ ", " ++ showIntLst xs

showLst :: (a -> String) -> [a] -> String
showLst _ []       = ""
showLst f [x     ] = f x
showLst f (x : xs) = f x ++ ", " ++ showLst f xs

-- 8.
divide :: Int -> String -> String
divide n = unlines . concatMap (\v -> if null v then [[]] else go v) . lines
  where
    go :: [a] -> [[a]]
    go [] = []
    go l  = take n l : go (drop n l)

main10 :: IO ()
main10 = interact $ divide 3

{-
To test the performance in ghci use:
:set +s
To stop:
:unset +s
-}
