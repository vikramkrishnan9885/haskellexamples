removeNonUpperCase :: [Char] -> [Char]
removeNonUpperCase st = [c | c <- st, c `elem` ['A'..'Z']]

addThreeNums :: Int -> Int -> Int -> Int
addThreeNums x y z = x + y + z

factorialInt :: Int -> Int
factorialInt n  = product[1..n]

factorial :: Integer -> Integer
factorial n = product [1..n]
