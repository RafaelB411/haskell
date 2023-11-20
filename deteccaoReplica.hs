module Main where

isReplica :: String -> Int -> Char -> Bool
isReplica [] 0 char = True
isReplica (a:as) num char | a /= char  = False
                          | num == 0   = False
                          | otherwise  = isReplica (as) (num-1) char

main = do
    a <- getLine
    b <- getLine
    c <- getChar
    let result = isReplica a (read b) c
    print result
