module Main where
    -- data CoolBool = CoolBool { getCoolBool :: Bool } deriving Show
    newtype CoolBool = CoolBool { getCoolBool :: Bool } deriving Show

    helloMe :: CoolBool -> String
    helloMe (CoolBool _) = "hello"

    main = do
        putStrLn $ helloMe undefined
