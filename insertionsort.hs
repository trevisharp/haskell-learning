insertion :: (Ord a) => [a] -> [a] -> [a]
insertion [] [] = []
insertion [] y = y
insertion (x:xs) y = insertion xs ((filter (<x) y) ++ [x] ++ (filter (>=x) y))

insertionsort :: (Ord a) => [a] -> [a]
insertionsort x = insertion x []