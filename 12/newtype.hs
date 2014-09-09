module Main where
    newtype Pair b a = Pair { getPair :: (a, b) } deriving Show

    instance Functor (Pair c) where
        fmap f (Pair (x,y)) = Pair (f x, y)

    main = do
            putStrLn $ show $ getPair $ fmap (*100) (Pair (2, 3))
            putStrLn $ show $ getPair $ fmap reverse (Pair ("london calling", 3))
