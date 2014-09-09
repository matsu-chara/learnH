module BinaryTree (Tree, singleton, 
                   fromList, toList,
                   search, searchMin, searchMax, 
                   insert, 
                   deleteMin, deleteMax, delete, 
                   foldLeft, foldRight) where

data Tree a = Nil | Node a (Tree a) (Tree a) deriving Show

-- $setup
-- >>> let a = Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)) (Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil))

-- | リストから二分木を作る
-- >>> fromList [3,2,4]
-- Node 3 (Node 2 Nil Nil) (Node 4 Nil Nil)
fromList :: Ord a => [a] -> Tree a
fromList = foldl (flip insert) Nil

-- | 二分木からリストを作る
-- >>> toList a
-- [1,2,3,4,5,6,7]
toList :: Ord a => Tree a -> [a]
toList tree = iter' tree [] where
  iter' Nil xs = xs
  iter' (Node x l r) xs = iter' l (x: iter' r xs)

-- | 要素がひとつの木
-- >>> singleton 3
-- Node 3 Nil Nil
singleton :: a -> Tree a
singleton x = Node x Nil Nil

-- | 要素の探索
-- >>> search 3 a
-- Just 3
search :: Ord a => a -> Tree a -> Maybe a
search _ Nil = Nothing
search x (Node y l r)
  | x == y = Just y
  | x <  y = search x l
  | otherwise = search x r

-- | 最小値の探索
-- >>> searchMin a
-- Just 1
searchMin :: Ord a => Tree a -> Maybe a
searchMin Nil = Nothing
searchMin (Node x Nil _) = Just x
searchMin (Node _   l _) = searchMin l

-- | 最大値の探索
-- >>> searchMax a
-- Just 7
searchMax :: Ord a => Tree a -> Maybe a
searchMax Nil = Nothing 
searchMax (Node x _ Nil) = Just x
searchMax (Node _ _   r) = searchMax r

-- | 要素の追加
-- >>> insert 11 a 
-- Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 11 Nil Nil)))
insert :: Ord a => a -> Tree a -> Tree a
insert x Nil = singleton x
insert x (Node y l r)
  | x == y    = Node x l r
  | x < y     = Node y (insert x l) r
  | otherwise = Node y l (insert x r)

-- | 最小値の削除
-- >>> deleteMin a
-- Node 4 (Node 2 Nil (Node 3 Nil Nil)) (Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil))
deleteMin :: Ord a => Tree a -> Tree a
deleteMin Nil = Nil
deleteMin (Node _ Nil r) = r
deleteMin (Node x l r) = Node x (deleteMin l) r

-- | 最大値の削除
-- >>> deleteMax a
-- Node 4 (Node 2 (Node 1 Nil Nil) (Node 3 Nil Nil)) (Node 6 (Node 5 Nil Nil) Nil)
deleteMax :: Ord a => Tree a -> Tree a
deleteMax Nil = Nil
deleteMax (Node _ l Nil) = l
deleteMax (Node x l r) = Node x l (deleteMax r)

-- | 要素の削除
-- >>> delete 3 a
-- Node 4 (Node 2 (Node 1 Nil Nil) Nil) (Node 6 (Node 5 Nil Nil) (Node 7 Nil Nil))
delete :: Ord a => a -> Tree a -> Tree a
delete x Nil = Nil
delete x (Node y l r) 
  | x < y = Node y (delete x l) r
  | x > y = Node y l (delete x r)
  | otherwise = delete' l r where
      delete' Nil r = r
      delete' l Nil = l
      delete' l r = Node x' l (deleteMin r)
        where Just x' = searchMin r

-- | 左畳み込み
-- >>> foldLeft (flip (:)) [] a
-- [7,6,5,4,3,2,1]
foldLeft :: (a -> b -> a) -> a -> Tree b -> a
foldLeft _ a Nil = a
foldLeft f a (Node x l r) = foldLeft f (f (foldLeft f a l) x) r

-- | 右畳み込み
-- >>> foldRight (:) [] a
-- [1,2,3,4,5,6,7]
foldRight :: (a -> b -> b) -> b -> Tree a -> b
foldRight _ a Nil = a
foldRight f a (Node x l r) = foldRight f (f x (foldRight f a r)) l

