bubble :: (Ord a) => [a] -> [a]
bubble [x] = [x]
bubble (x:y:xs) = if x < y then x:(bubble (y:xs)) else y:(bubble (x:xs))

bubblesort :: (Ord a) => [a] -> [a]
bubblesort x = foldl (\xs _ -> bubble x) x x