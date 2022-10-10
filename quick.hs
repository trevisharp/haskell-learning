quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted

smartsort :: (Ord a) => [a] -> [a]    
smartsort [] = []    
smartsort (x:xs) =     
    let smallerSorted = smartsort (filter (<=x) xs)  
        biggerSorted = smartsort (filter (>x) xs)   -- :t (>3) :: (Ord a, Num a) => a -> Bool
    in  smallerSorted ++ x:biggerSorted


mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = f x : mymap f xs


chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)

numLongChains n = length (filter (\xs -> length xs > 15) (map chain [1..n]))
