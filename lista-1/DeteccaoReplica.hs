module Main where

isReplica :: String -> Int -> Char -> Bool
-- points if a string is a given char repeated n times
isReplica [] 0 _ = True
isReplica [] n _ = False
isReplica (a:as) 0 char | a == char  = False
                        | otherwise  = isReplica (as) 0 char
isReplica (a:as) n char | a /= char  = False
                          | otherwise  = isReplica (as) (n-1) char

main = do
    a <- getLine
    b <- getLine
    c <- getChar
    let result = isReplica a (read b) c
    print result
