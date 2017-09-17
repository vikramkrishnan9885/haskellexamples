initials::String -> String -> String
initials firstname lastname = [f]++ "."++ [l]
    where   (f:_)=firstname
            (l:_)=lastname

{- }initials2 :: String -> String -> String
initials2 firstname2 lastname2 = [f2]++"."++[l2]
(f2:_) = firstname2
(l2:_) = lastname2
-}
initials3 :: String -> String -> String
initials3 firstname3 lastname3 = [f3]++"."++[l3]
    where   f3 = head firstname3
            l3 = head lastname3

initials4 :: String -> String -> String
initials4 firstname4 lastname4 =
    let     f4 = head firstname4
            l4 = head lastname4
    in      [f4] ++ [l4]

initialsExtractor :: String -> String
initialsExtractor x = [head x]

initials5 :: String -> String -> String 
initials5 firstname5 lastname5 = initialsExtractor firstname5 ++ initialsExtractor lastname5

initials6 :: String -> String -> String
initials6 firstname6 lastname6 = do
    f6 <- initialsExtractor firstname6
    l6 <- initialsExtractor lastname6
    a6 <- f6:l6:[]
    return(a6)

initialsExtractor2 :: String -> String
initialsExtractor2 x = 
    let     y = head x
    in      [y]

initials7 :: String -> String -> String
initials7 firstname7 lastname7 = do
    f7 <- (initialsExtractor firstname7:[])
    l7 <- (initialsExtractor lastname7:[])
    a7 <- f7 ++ l7 
    return(a7)
