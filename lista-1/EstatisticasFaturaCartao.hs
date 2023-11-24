split :: Char -> String -> [String]
-- splits a string into substrings when of occurrence 
-- of a given char
split _ [] = []
split key string =  let (segment,string') = break (== key) string
                     in segment : case string' of
                           [] -> []
                           (_:string'') -> split key string''

valueList :: [String] -> [Double]
-- isolates the money amounts present on the string 
-- into a double array 
valueList [] = []
valueList [[]] = []
valueList (a:b:c:strings) = read c : valueList (strings)

minValue :: [Double] -> Double
-- returns the minimum value on the given double array
minValue [] = 0
minValue [x] = x
minValue (x:y:values) | x > y     = minValue (y:values)
                      | otherwise = minValue (x:values)

maxValue :: [Double] -> Double
-- returns the maximum value on the given double array
maxValue [] = 0
maxValue [x] = x
maxValue (x:y:values) | x > y     = maxValue (x:values)
                      | otherwise = maxValue (y:values)

minMaxCartao :: String -> (Double,Double)
-- returns a tuple with minimum and maximum amounts 
-- present on the array
minMaxCartao [] = (0,0)
minMaxCartao fatura = (minValue (valueList (split ';' fatura)),
                       maxValue (valueList (split ';' fatura))) 

main = do 
    a <- getLine
    let result = minMaxCartao a
    print result
