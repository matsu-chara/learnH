import Data.Char
import Data.List
main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
          putStrLn line

       -- do line <- fmap (\xs -> intersperse '-' . (reverse (map toUpper xs))) getLine
