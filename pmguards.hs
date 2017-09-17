factorial' :: Integer -> Integer
factorial' n
    | n < 0 = 0
factorial' 0 = 1  
factorial' n = n * factorial' (n - 1)  
    

factorial2 :: (Integral a)  => a -> a
factorial2 n
    | n < 0 = 0
factorial2 0 = 1  
factorial2 n = n * factorial2 (n - 1)  
