module Main where

isReplica :: String -> Int -> Char -> Bool
isReplica [] 0 _ = True
isReplica [] num _ = False
isReplica (a:as) 0 char | a == char  = False
                        | otherwise  = isReplica (as) 0 char
isReplica (a:as) num char | a /= char  = False
                          | otherwise  = isReplica (as) (num-1) char

main = do
    a <- getLine
    b <- getLine
    c <- getChar
    let result = isReplica a (read b) c
    print result
