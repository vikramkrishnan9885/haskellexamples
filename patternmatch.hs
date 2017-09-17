lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"

factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)

charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil"  

charName' :: Char -> [Char]  
charName' 'a' = "Albert"  
charName' 'b' = "Broseph"  
charName' 'c' = "Cecil"  
charName' x = "Who are you?"

addVec :: (Num a) => (a,a) -> (a,a) -> (a,a)
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 

first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z  

head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x  
