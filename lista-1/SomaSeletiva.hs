getNumber :: Char -> Char -> Int
-- translate char into numbers
getNumber a b = (fromEnum a) - (fromEnum b)

sumNumbers :: String -> Int
-- sum numbers on a string
sumNumbers [] = 0
sumNumbers (a:as) | a >= '0' && a <= '9' = (getNumber a '0') + sumNumbers as
                  | otherwise            = sumNumbers as

main = do 
       a <- getLine
       let result = sumNumbers a 
       print result
