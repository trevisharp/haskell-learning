merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge (x:xs) [] = (x:xs)
merge [] (y:ys) = (y:ys)
merge (x:xs) (y:ys) = if x < y then x:(merge xs (y:ys)) else y:(merge (x:xs) ys)

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort x = merge (mergesort $ take len x) (mergesort $ drop len x)
    where len = div (length x) 2