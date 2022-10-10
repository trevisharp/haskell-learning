import Sort

showSort :: (Show a, Ord a) => [a] -> [String]
showSort x = map show (Sort.mergesort x)