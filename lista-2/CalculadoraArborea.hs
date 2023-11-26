data Ops = SUM | MUL | SUB
           deriving (Read)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read)

evalTree :: IntTree -> Int


main = do 
       s <- getLine
       let result = evalTree (read s)
       print result
