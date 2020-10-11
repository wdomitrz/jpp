module Lab05transformers where
import           Control.Monad.Trans.State
import           Data.Char                      ( digitToInt
                                                , isDigit
                                                )
import           Control.Monad.Trans.Class      ( MonadTrans(..) )
import           Control.Monad                  ( MonadPlus(..) )
import           Control.Applicative            ( Alternative(..)
                                                , many
                                                )
import           Data.Maybe                     ( fromMaybe )
-- 1.
type Parser a = StateT String Maybe a
runParser :: Parser a -> String -> Maybe (a, String)
runParser = runStateT

-- | item
-- >>> runParser item "ela"
-- Just ('e',"la")
-- >>> runParser item ""
-- Nothing
item :: Parser Char
item = do
    (x : xs) <- get
    put xs
    return x

-- | sat
-- >>> runParser (sat $ flip elem "abc") "ala"
-- Just ('a',"la")
-- >>> runParser (sat $ flip elem "abc") "ela"
-- Nothing
sat :: (Char -> Bool) -> Parser Char
sat f = do
    x <- item
    if f x then return x else fail "Condition not satisfied"

-- | many (imported from Applicative) and many1
-- >>> runParser (many $ char 'e') "eeela"
-- Just ("eee","la")
-- >>> runParser (many $ char 'e') "ala"
-- Just ("","ala")
-- >>> runParser (many1 $ char 'e') "eeela"
-- Just ("eee","la")
-- >>> runParser (many1 $ char 'e') "ala"
-- Nothing
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

-- | pDigit
-- >>> runParser pDigit "123"
-- Just (1,"23")
-- >>> runParser pDigit "ela"
-- Nothing
pDigit :: Parser Int
pDigit = digitToInt <$> sat isDigit

-- | pNat
-- >>> runParser pNat "123ela"
-- Just (123,"ela")
-- >>> runParser pNat "ela"
-- Just (0,"ela")
pNat :: Parser Int
pNat = sum . zipWith ((*) . (10 ^)) [0 ..] . reverse <$> pDigits

-- Additional functions
-- >>> runParser pDigits "123ela"
-- Just ([1,2,3],"ela")
-- >>> runParser pDigits "ela"
-- Just ([],"ela")
pDigits :: Parser [Int]
pDigits = many pDigit

-- | pDigit1
-- > runParser pDigit1 "123"
-- Just (1,"23")
-- > runParser pDigit1 "ela"
-- Nothing
pDigit1 :: Parser Int
pDigit1 = do
    x <- item
    if isDigit x then return $ digitToInt x else fail ""

-- | char
-- > runParser (char 'e') "ela"
-- Just ('e',"la")
-- > runParser (char 'f') "ela"
-- Nothing
char :: Char -> Parser Char
char = sat . (==)

-- 2.
newtype IdentityT f a = IdentityT {runIdentityT :: f a}

instance Functor m => Functor (IdentityT m) where
    fmap f = IdentityT . fmap f . runIdentityT

instance (Monad m) => Monad (IdentityT m) where
    (>>=) x f = IdentityT (runIdentityT x >>= runIdentityT . f)

instance MonadTrans IdentityT where
    lift = IdentityT

instance MonadPlus m => MonadPlus (IdentityT m)

-- Needed for Monad
instance Applicative m => Applicative (IdentityT m) where
    pure = IdentityT . pure
    (<*>) f x = IdentityT $ runIdentityT f <*> runIdentityT x

-- Needed for MonadPlus
instance Alternative m => Alternative (IdentityT m) where
    empty = IdentityT empty
    (<|>) x y = IdentityT $ runIdentityT x <|> runIdentityT y

-- 3.
newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
    fmap f = MaybeT . fmap (fmap f) . runMaybeT
--                           ^ Maybe is a Functor instance

instance Monad m => Monad (MaybeT m) where
    (>>=) x f =
        MaybeT $ runMaybeT x >>= foldr (const . runMaybeT . f) (return Nothing)
--                                ^ Maybe is a Foldable instance

instance MonadTrans MaybeT where
    lift = MaybeT . (Just <$>)

instance Monad m => MonadPlus (MaybeT m)

-- Needed for Monad
instance Applicative m => Applicative (MaybeT m) where
    pure = MaybeT . pure . Just
    (<*>) f x = MaybeT $ (<*>) <$> runMaybeT f <*> runMaybeT x
--                         ^ Maybe is an Applicative instance

-- Needed for Alternative
instance Applicative m => Alternative (MaybeT m) where
    empty = MaybeT $ pure Nothing
    (<|>) x y = MaybeT $ (<|>) <$> runMaybeT x <*> runMaybeT y
--                         ^ Maybe is an Alternative instance
