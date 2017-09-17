{-[x | x<-[1..50], '7' `elem` show x]-}

class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

instance MonadPlus [] where
    mzero = []
    mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

{-[1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
-}
{-guard (5 > 2) >> show "cool" :: [Char]-}

sevensOnly :: [Int]
sevensOnly = do
    x <- [1..50]
    guard ('7' `elem` show x)
    return x

{-[ x | x <- [1..50], '7' `elem` show x ]-}
