split :: Char -> String -> [String]
split _ [] = []
split key str = let (seg,str') = break (== key) str
                 in seg : case str' of
                          [] -> []
                          (_:str'') -> split key str''

listMonthValues :: [String] -> [(String,Double)]
listMonthValues [] = []
listMonthValues (a:b:c:segs) = let (_,a') = break (== ' ') a 
                              in case a' of
                                 (_:a'') -> (a'',read c) : listMonthValues segs

filterValues :: String -> [(String,Double)] -> [Double]
filterValues _ [] = []
filterValues key ((month,val):tuples) | month == key  = val : filterValues key tuples
                                      | otherwise     = filterValues key tuples

logMes :: String -> String -> Double
logMes key str = foldl (+) 0 (filterValues key (listMonthValues (split ';' str)))

main = do
     a <- getLine
     b <- getLine
     let return = logMes a b
     print return
