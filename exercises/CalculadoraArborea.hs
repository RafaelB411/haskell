data Ops = SUM | MUL | SUB
           deriving (Read)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read)

getOp :: Ops -> (Int -> Int -> Int)
-- compute given operation
getOp SUM = (+)
getOp MUL = (*)
getOp SUB = (-)

evalTree :: IntTree -> Int
-- return the result of given operations
evalTree (Nilt n) = n 
evalTree (Node op ltree rtree) = (getOp op) (evalTree ltree) (evalTree rtree)

main = do 
       s <- getLine
       let result = evalTree (read s)
       print result
