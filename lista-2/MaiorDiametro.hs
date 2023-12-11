data Tree t = Nilt 
            | Node t (Tree t) (Tree t)
            deriving (Read, Show)

altura :: Tree t -> Int 
-- return the height of a tree
altura Nilt = 0
altura (Node t subtree1 subtree2) = 1 + (max (altura subtree1) (altura subtree2))

diametro :: Ord t => Tree t -> Tree Int 
-- assign to each node the greatest distance between nodes 
-- from its subtrees
diametro Nilt = Node 0 Nilt Nilt
diametro (Node t subtree1 subtree2) = Node (1 + (altura subtree1) + (altura subtree2)) (diametro subtree1) (diametro subtree2)

maxNode :: Tree Int -> Int
-- return the biggest value assigned to nodes of a tree 
maxNode Nilt = 0
maxNode (Node t subtree1 subtree2) = max t (max (maxNode subtree1) (maxNode subtree2))

maiorDiametro :: Ord t => Tree t -> Int 
-- combine maxNode and diametro functios
maiorDiametro Nilt = 0
maiorDiametro (Node t subtree1 subtree2) = maxNode (diametro (Node t subtree1 subtree2))

main = do 
       s <- getLine
       let result = maiorDiametro (read s::Tree Int)
       print result