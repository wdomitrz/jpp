import           Text.Read                      ( readEither )
import           System.Environment             ( getArgs )
import           Control.Monad                  ( when )
-- 1.
swapEither :: Either e a -> Either a e
swapEither = either Right Left

readInts2 :: String -> Either String [Int]
readInts2 = foldr go (return []) . words
  where
    go :: String -> Either String [Int] -> Either String [Int]
    go s = (<*>) (go' (readEither s) >>= (Right . (:)))
      where
        go' :: Either String Int -> Either String Int
        go' =
            swapEither
                . (>>= (const $ return ("Not a number: " ++ s)))
                . swapEither

readInts2' :: String -> Either String [Int]
readInts2' = foldr go (return []) . words
  where
    go :: String -> Either String [Int] -> Either String [Int]
    go s l = do
        newInt <- (go' . readEither) s
        fmap (newInt :) l
      where
        go' :: Either String Int -> Either String Int
        go' x = swapEither $ do
            _ <- swapEither x
            return ("Not a number: " ++ s)



sumInts :: String -> String
sumInts = either id (show . sum) . readInts2

sumInts' :: String -> String
sumInts' s = either id id $ do
    ints <- readInts2 s
    return $ show $ sum ints

main1 :: IO ()
main1 = interact sumInts

-- 2.
-- a.
main2a :: IO ()
main2a = getArgs >>= mapM_ putStrLn

-- b.
main2b :: IO ()
main2b =
    putStrLn "What is your favourite programming language?"
        >>= const getLine
        >>= (flip when main2b . ("Haskell" /=))

main2b' :: IO ()
main2b' = do
    putStrLn "What is your favourite programming language?"
    language <- getLine
    when ("Haskell" /= language) main2b

-- c.
main2c :: IO ()
main2c =
    getArgs
        >>= (\args -> if null args then getContents else readFile $ head args)
        >>= (print . go)
  where
    go :: String -> (Int, Int, Int)
    go s = (length $ lines s, length $ words s, length s)

main2c' :: IO ()
main2c' = do
    args    <- getArgs
    content <- if null args then getContents else readFile $ head args
    print $ go content
  where
    go :: String -> (Int, Int, Int)
    go s = (length $ lines s, length $ words s, length s)
