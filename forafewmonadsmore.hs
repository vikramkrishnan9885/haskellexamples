import Data.Monoid

isBigGang1 :: Int -> Bool
isBigGang1 x = x > 9

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared to a gang sized 9")

applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)
applyLog (x, log) f = let (y,newLog) = f x in (y,log ++ newLog)

example :: String -> (Int, String)
example a = (length a, "Applied Length")

applyLog1 :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)
applyLog1 (x,log) f = let (y, newLog) = f x in (y, log `mappend` newLog) 
