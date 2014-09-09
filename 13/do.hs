module Main where
    foo :: Maybe String
    foo = do
        x <- Just 3
        y <- Just "!"
        Just (show x ++ y)

    main = do
        print $ Just 3 >>= (\x -> Just (show x ++ "!"))
        print $ foo
