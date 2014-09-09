module Main where
    import Control.Applicative

    applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
    applyMaybe Nothing f = Nothing
    applyMaybe (Just x) f = f x

    main = do
        -- print $ fmap (++"!") (Just "wisdom")
        -- print $ fmap (++"!") Nothing
        -- print $ Just (+3) <*> Just 3
        -- print $ Just (+3) <*> Nothing
        -- print $ max <$> Just 3 <*> Just 6
        -- print $ max <$> Just 3 <*> Nothing
        print $ applyMaybe (Just 3) (\x -> Just (x+1))
        print $ applyMaybe (Just 1) (\x -> if x > 2 then Just x else Nothing)
