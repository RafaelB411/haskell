data Tree t = Node t (Tree t) (Tree t)
              | Nilt
              deriving (Read)

maxi :: Int -> Int -> Int
-- return max value
maxi x y | x > y     = x
         | otherwise = y

alturaArvore :: Tree t -> Int
-- return the height of a tree
alturaArvore Nilt = 0
alturaArvore (Node n ltree rtree) = 1 + (maxi (alturaArvore ltree) (alturaArvore rtree))

main = do 
       a <- getLine
       let result = alturaArvore (read a::Tree Int)
       print result
