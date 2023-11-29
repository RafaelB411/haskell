sumLoop :: Int -> Int -> Int
-- sum all of a number till a given factor
sumLoop a iter | iter <= 0 = 0
               | otherwise = a * iter + sumLoop a (iter-1)

somarMultiplos :: [Int] -> Int -> [Int]
-- return a list with the sum of all multiples of a given
-- that are less than a given value
somarMultiplos [] _ = []
somarMultiplos (a:as) 0 = 0 : somarMultiplos as 0
somarMultiplos (a:as) n = sumLoop n (a `div` n) : somarMultiplos as n

main = do
       lista <- getLine
       let readList = read lista :: [Int]
       num <- getLine
       let readNum = read num :: Int
       let result = somarMultiplos readList readNum
       print result
