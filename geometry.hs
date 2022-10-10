data Point = Point {x :: Float, y:: Float} deriving (Show)

infixr 5 .+
(.+) :: Point -> Point -> Point
(.+) p q = Point ((x p) + (x q)) ((y p) + (y q))

infixr 5 .-
(.-) :: Point -> Point -> Point
(.-) p q = Point ((x p) - (x q)) ((y p) - (y q))

class Shape s where
    area :: s -> Float

data Rectangle = Rectangle Point Point deriving (Show)
data Circle = Circle Point Float deriving (Show)

instance Shape Rectangle where
    area (Rectangle p q) = let r = p .- q in (abs $ x r) * (abs $ y r)

instance Shape Circle where
    area (Circle _ r) = pi * r ^ 2

-- bigger :: (Shape a) => a -> a -> a
-- bigger x y = if (area x) > (area y) then x else y