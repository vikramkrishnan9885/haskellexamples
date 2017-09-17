data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
{- Circle takes three inputs x coordinate of center, y coordinate of center and radius. While rectangle takes the upper left and lower right coordinates.Deriving show lets display the object -}

surface :: Shape -> Float
surface (Circle _ _ r) = pi *r ^2
surface (Rectangle x1 y1 x2 y2) = (abs (x2-x1))*(abs (y2-y1))


data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)

surface' :: Shape' -> Float
surface' (Circle' _ r) = pi * r ^ 2
surface' (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs $ x2 -x1) * (abs $ y2-y1)
