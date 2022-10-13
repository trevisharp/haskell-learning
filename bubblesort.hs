bubble :: (Ord a) => [a] -> [a]
bubble [x] = [x]
bubble [x, y] = if x < y then [x, y] else [y, x]
bubble (x:y:xs) = if x < y then x:(bubble (y:xs)) else y:(bubble (x:xs))

bubblestep :: (Ord a) => [a] -> Int -> [a]
bubblestep x i = if i > 0 then bubblestep (bubble x) (i - 1) else bubble x

bubblesort :: (Ord a) => [a] -> [a]
bubblesort x = bubblestep x (length x)