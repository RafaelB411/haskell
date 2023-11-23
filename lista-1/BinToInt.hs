module Main where

btoi :: String -> Int
btoi [] = 0
btoi (a:as) | a == '1'  = 2^(length as) + btoi (as)
            | a == '0'  = btoi (as)

main = do
    s <- getLine
    let result = btoi s
    print result
