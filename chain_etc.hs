chain :: Integer -> [Integer]
chain 1 = [1]
chain n
    | even n = n : chain (n `div` 2)
    | odd  n = n : chain (n * 3 + 1)


numLongChains :: Int -> Int
numLongChains  x = length ( filter (isLong x) (map chain [1..100]))
    where isLong x xs = length xs > x

numLongChains5 = numLongChains 5

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a]-> [a]
reverse' = foldl(\acc x -> x : acc) []

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

