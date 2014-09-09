module Main where
    import Data.Monoid

    lengthCompare :: String -> String -> Ordering
    lengthCompare x y = (length x `compare` length y) `mappend`
                        (vowels x `compare` vowels y) `mappend`
                        (x `compare` y)
                        where vowels = length . filter ( `elem` "aeiou")

    main = do
        print $ lengthCompare "zen" "ants"
        print $ lengthCompare "zen" "ana"
        print $ lengthCompare "zen" "ann"

