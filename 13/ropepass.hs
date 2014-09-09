module Main where
    type Birds = Int
    type Pole = (Birds, Birds)

    landLeft' :: Birds -> Pole -> Pole
    landLeft' n (left, right) = (left + n, right)

    landRight' :: Birds -> Pole -> Pole
    landRight' n (left, right) = (left, right + n)

    -- Maybe版
    landLeft :: Birds -> Pole -> Maybe Pole
    landLeft n (left, right)
        | abs ((left + n) - right) < 4 = Just (left + n, right)
        | otherwise                    = Nothing

    landRight :: Birds -> Pole -> Maybe Pole
    landRight n (left, right)
        | abs (left - (right + n)) < 4 = Just (left, right + n)
        | otherwise                    = Nothing

    -- >>=っぽい形式で書けるようにするための関数
    x -: f = f x

    banana :: Pole -> Maybe Pole
    banana _ = Nothing

    main = do
        -- print $ landRight 2 (0, 0)
        -- print $ landLeft 2 (0, 2)
        -- print $ landLeft 10 (0, 2)
        -- print $ landLeft 4 $ landLeft 10 (0, 2) -- $以降がMaybe Pole型が出るのでエラーになる
        -- print $ landLeft 10 (0, 2) >>= landLeft 2 -- モナド値を普通の値をとる関数に渡せる！
        print $ (0, 0) -: landLeft' 1 -: landRight' 4 -: landLeft' (-1) -: landRight' (-2)
        print $ return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
        print $ return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
        print $ return (0,0) >>= landLeft 1 >>= banana >>= landRight 2
