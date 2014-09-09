module Main where
    import Control.Monad
    sevensOnly :: [Int]
    sevensOnly = do
        x <- [1..50]
        guard (elem '7' (show x))
        return x

    main = do
        print $ sevensOnly
        print $ [ x | x <- [1..50], elem '7' (show x)]
