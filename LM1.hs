module LM1 where -- required by doctest
import           Text.Read                      ( readEither )
import           System.Environment             ( getArgs )
import           Control.Monad                  ( when
                                                , foldM
                                                )
import           Control.Monad.Error.Class
import           Data.Char                      ( isHexDigit
                                                , digitToInt
                                                )
-- 1.
swapEither :: Either e a -> Either a e
swapEither = either Right Left

-- | readInts2
-- >>> readInts2 "1 23 456 abc 9"
-- Left "Not a number: abc"
-- >>> readInts2' "1 23 456 abc 9"
-- Left "Not a number: abc"
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

-- | sumInts
-- >>> sumInts "1 23 456 abc 9"
-- "Not a number: abc"
-- >>> sumInts "1 2 3"
-- "6"
-- >>> sumInts' "1 23 456 abc 9"
-- "Not a number: abc"
-- >>> sumInts' "1 2 3"
-- "6"
sumInts :: String -> String
sumInts = either id (show . sum) . readInts2

sumInts' :: String -> String
sumInts' s = either id id $ do
    ints <- readInts2 s
    return $ show $ sum ints

main1 :: IO ()
main1 = interact sumInts

-- 2.
data ParseError = Err {location::Int, reason::String} deriving Show -- pokazywanie do celów debugowych

type ParseMonad = Either ParseError
parseHexDigit :: Char -> Int -> ParseMonad Integer
parseHexDigit c l
    | isHexDigit c = return $ toInteger $ digitToInt c
    | otherwise = throwError
    $ Err { location = l, reason = show c ++ " is not a hex digit" }
parseHex :: String -> ParseMonad Integer
parseHex = snd . foldl
    (\(l, y) x -> (l + 1, (+) <$> ((16 *) <$> y) <*> parseHexDigit x l))
    (1, return 0)
parseHex' :: String -> ParseMonad Integer
parseHex' = (<$>) snd . foldM
    (\(l, y) -> (<$>) (\x' -> (l + 1, 16 * y + x')) . flip parseHexDigit l)
    (1, 0)
parseHex'' :: String -> ParseMonad Integer
parseHex'' s =
    (<$>) (foldl (\y x -> 16 * y + x) 0)
        $ fmap (map snd)
        $ mapM (\(l, y) -> (<$>) (curry id (l + 1)) $ parseHexDigit y l)
        $ zip [1 ..] s


toString :: Integer -> ParseMonad String
toString = return . show

-- convert zamienia napis z liczba szesnastkowa
--   na napis z liczba dziesiętna
convert :: String -> String
convert s = str  where
    (Right str) = tryParse s `catchError` printError
    tryParse s' = do
        n <- parseHex s'
        toString n
    printError e = return $ concat [show $ location e, ": ", reason e]

-- 3.
-- a.
main3a :: IO ()
main3a = getArgs >>= mapM_ putStrLn

-- b.
main3b :: IO ()
main3b =
    putStrLn "What is your favourite programming language?"
        >>= const getLine
        >>= (flip when main3b . ("Haskell" /=))

main3b' :: IO ()
main3b' = do
    putStrLn "What is your favourite programming language?"
    language <- getLine
    when ("Haskell" /= language) main3b'

-- c.
main3c :: IO ()
main3c =
    getArgs
        >>= (\args -> if null args then getContents else readFile $ head args)
        >>= (print . go)
  where
    go :: String -> (Int, Int, Int)
    go s = (length $ lines s, length $ words s, length s)

main3c' :: IO ()
main3c' = do
    args    <- getArgs
    content <- if null args then getContents else readFile $ head args
    print $ go content
  where
    go :: String -> (Int, Int, Int)
    go s = (length $ lines s, length $ words s, length s)

-- 4.
sequence' :: Monad m => [m a] -> m [a]
sequence' = foldr ((<*>) . (<$>) (:)) (return [])
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' = (sequence' .) . map
forM' :: Monad m => [a] -> (a -> m b) -> m [b]
forM' = flip mapM'

-- 5.
class Functor f => Applicative' f where
  pure' :: a -> f a
  (<**>) :: f(a->b) -> f a -> f b

instance Applicative' Maybe where
    pure' = Just
    (<**>) Nothing  _        = Nothing
    (<**>) _        Nothing  = Nothing
    (<**>) (Just f) (Just x) = Just $ f x
    --(<**>) mf mx = mf >>= (<$> mx)

instance Applicative' (Either e) where
    pure' = Right
    (<**>) (Left e)  _         = Left e
    (<**>) _         (Left  e) = Left e
    (<**>) (Right f) (Right x) = Right $ f x
    --(<**>) mf mx = mf >>= (<$> mx)

-- b.
(**>) :: Applicative' f => f a -> f b -> f b
(**>) = (<**>) . (<**>) (pure' (const id))

-- c.
(<**) :: Applicative' f => f a -> f b -> f a
(<**) = (<**>) . fmap const
