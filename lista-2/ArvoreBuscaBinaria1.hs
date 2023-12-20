data Tree t = Nilt
            | Node t (Tree t) (Tree t)
            deriving (Read)

checkTree1 :: Ord t => Tree t -> t -> Bool
checkTree1 Nilt key = True
checkTree1 (Node n subtree1 subtree2) key = (n < key) && (checkTree1 subtree1 key) && (checkTree1 subtree2 key) 

checkTree2 :: Ord t => Tree t -> t -> Bool
checkTree2 Nilt key = True
checkTree2 (Node n subtree1 subtree2) key = (n > key) && (checkTree2 subtree1 key) && (checkTree2 subtree2 key)

isBST :: Ord t => Tree t -> Bool
isBST Nilt = True
isBST (Node n Nilt Nilt) = True
isBST (Node n Nilt (Node p subtree21 subtree22)) = (checkTree2 (Node p subtree21 subtree22) n) && (isBST (Node p subtree21 subtree22))
isBST (Node n (Node m subtree11 subtree12) Nilt) = (checkTree1 (Node m subtree11 subtree12) n) && (isBST (Node m subtree11 subtree12))
isBST (Node n (Node m subtree11 subtree12) (Node p subtree21 subtree22)) = 
    (checkTree1 (Node m subtree11 subtree12) n) && (checkTree2 (Node p subtree21 subtree22) n) && (isBST (Node m subtree11 subtree12)) && (isBST (Node p subtree21 subtree22))

main = do
       s <- getLine
       let result = isBST (read s::Tree Int)
       print result
