data Tree a = Node a (Tree a) (Tree a)
            | Empty

height :: Tree a -> Int
height (Node _ lc rc) = max (height lc) (height rc) + 1
height Empty = 0
                        
