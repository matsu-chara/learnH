module Main where
    import Control.Monad

    type KnightPos = (Int, Int)

    moveKnight :: KnightPos -> [KnightPos]
    moveKnight (c,r) = do
        (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c+2,r+1),
                     (c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
        guard (c' `elem` [1..8] && r' `elem` [1..8])
        return (c', r')

    in3 :: KnightPos -> [KnightPos]
    in3 start = do
        first <- moveKnight start
        second <- moveKnight first
        moveKnight second

    isin3 :: KnightPos -> KnightPos -> Bool
    isin3 start goal = elem goal (in3 start)

    main = do
        -- print $ in3 (6,2)
        print $ isin3 (6,2) (0,4)
