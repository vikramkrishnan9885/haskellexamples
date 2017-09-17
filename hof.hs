zipWith' :: (a->b->c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
-- zipWith' [] _ _ = [] THIS WAS WRONG. LOOK AT TYPE SIGNATURE
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)


numLongChains :: Int
numLongChains =  length (filter (\xs -> length xs > 15)(map chain [1..100]))

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc +x) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x==y then True else acc) False ys

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile(<10000)(filter odd(map (^2)[1..])))

oddSquareSum'::Integer
oddSquareSum' = sum.takeWhile(<10000).filter odd.map(^2) $ [1..]

oddSquareSum2 :: Integer
oddSquareSum2 =
    let    oddSquares = filter odd $ map(^2) [1..]
           belowLimit = takeWhile(<10000) oddSquares
    in     sum belowLimit
