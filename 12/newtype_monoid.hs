module Main where
    import Data.Monoid

    main = do
        let a = getProduct $ Product 3 `mappend` Product 9
        let b = getProduct $ Product 3 `mappend` mempty
        let c = getProduct . mconcat . map Product $ [3,4,2]

        let d = getAny $ Any True `mappend` Any False
        let e = getAny $ mempty `mappend` Any False
        let f = getAny . mconcat . map Any $ [False, False, True]

        let g = getAll $ All True `mappend` All True
        let h = getAll . mconcat . map All $ [False, False, True]
        print h
