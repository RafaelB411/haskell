split :: Char -> String -> [String]
split _ [] = []
split key string =  let (segment,string') = break (== key) string
                     in segment : case string' of
                           [] -> []
                           (_:string'') -> split key string''

valueList :: [String] -> [Double]
valueList [] = []
valueList [[]] = []
valueList (a:b:c:strings) = read c : valueList (strings)

minValue :: [Double] -> Double
minValue [] = 0
minValue [x] = x
minValue (x:y:values) | x > y     = minValue (y:values)
                      | otherwise = minValue (x:values)

maxValue :: [Double] -> Double
maxValue [] = 0
maxValue [x] = x
maxValue (x:y:values) | x > y     = maxValue (x:values)
                      | otherwise = maxValue (y:values)

minMaxCartao :: String -> (Double,Double)
minMaxCartao [] = (0,0)
minMaxCartao fatura = (minValue (valueList (split ';' fatura)),
                       maxValue (valueList (split ';' fatura))) 

main = do 
    a <- getLine
    let result = minMaxCartao a
    print result
