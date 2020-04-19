{- A Binary Search Tree -}
module BST (
    ) where

data Tree a = Node a (Tree a) (Tree a)
              | Nil
              deriving (Show)

empty :: Tree a
empty = Nil

value :: Tree a -> Maybe a
value Nil = Nothing
value (Node elem _ _) = Just elem

left :: Tree a -> Maybe (Tree a)
left Nil = Nothing
left (Node _ l _) = Just l

right :: Tree a -> Maybe (Tree a)
right Nil = Nothing
right (Node _ _ r) = Just r

insert :: (Ord a) => a -> Tree a -> Tree a
insert newElem Nil = Node newElem (Nil) (Nil)
insert newElem (Node elem l r)
    | newElem < elem = Node elem (insert newElem l) r
    | otherwise      = Node elem l (insert newElem r)

