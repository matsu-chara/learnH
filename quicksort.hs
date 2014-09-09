quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    --let smallerOrEqual = [a | a<-xs, a<=x]
        --larger         = [a | a<-xs, a >x]
    --in  quicksort smallerOrEqual ++ [x] ++ quicksort larger

    let smallerOrEqual = filter (<= x) xs
        larger         = filter (>  x) xs
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger