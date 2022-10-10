sqrtrec x pi ps = if ps * ps - x < 0.00001
    then (if ps * ps - x > -0.00001
        then ps
        else sqrtrec x ps (2 * ps))
    else sqrtrec x pi ((pi + ps) / 2)

mysqrt :: Float -> Float
mysqrt x = sqrtrec x 0 x

circumference :: Double -> Double  
circumference r = 2 * pi * r

lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!" 

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors a b = (fst a + fst b, snd a + snd b)

add3Nums :: (Num x) => x -> x -> x -> x
add3Nums x y z = x + y + z

myhead :: [a] -> a  
myhead [] = error "Can't call head on an empty list, dummy!"  
myhead (x:_) = x

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0

describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  

mymax :: (Ord a) => [a] -> a  
mymax [] = error "maximum of empty list"  
mymax [x] = x  
mymax (x:xs)
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = mymax xs