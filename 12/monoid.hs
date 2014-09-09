module Main where
    import Data.Monoid

    main = do
        print $ [1,2,3] `mappend` [4,5,6]
        print $ ("one" `mappend` "two") `mappend` "three"
        print $ "one" `mappend` ("two" `mappend` "three")
        print $ "one" `mappend` "two" `mappend` "three"

