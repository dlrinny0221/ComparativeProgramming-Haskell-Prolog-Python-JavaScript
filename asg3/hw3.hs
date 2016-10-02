{-
 - Authors: Yunyi Ding and Brian Lin
 -}

data BST k v = Empty | Node k v (BST k v) (BST k v)
val :: BST k v -> Maybe v
val Empty = Nothing
val (Node k v _ _) = Just v

size :: BST k v -> Int
size Empty = 0;
size (Node k v l r) = 1 + size l + size r