module RedBlack () where
date Color = R | B
data RedBlackTree a = Nil | Node Color a (RedBlackTree a) (RedBlackTree a)
