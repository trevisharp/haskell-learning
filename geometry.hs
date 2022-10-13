type Point = (Float, Float)

infixr 5 .+
(.+) :: Point -> Point -> Point
(.+) p q = ((fst p) + (fst q), (snd p) + (snd q))

infixr 5 .-
(.-) :: Point -> Point -> Point
(.-) p q = ((fst p) - (fst q), (snd p) - (snd q))

class Shape s where
    area :: s -> Float

data Rectangle = Rectangle Point Point deriving (Show)
data Circle = Circle Point Float deriving (Show)

instance Shape Rectangle where
    area (Rectangle p q) = let r = p .- q in (abs $ fst r) * (abs $ snd r)

instance Shape Circle where
    area (Circle _ r) = pi * r ^ 2