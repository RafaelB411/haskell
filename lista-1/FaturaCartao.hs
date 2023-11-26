split :: Char -> String -> [String]
-- split a string into substrings when of occurrence 
-- of a given char
split _ [] = []
split key str = let (seg,str') = break (== key) str
                 in seg : case str' of
                          [] -> []
                          (_:str'') -> split key str''

listMonthValues :: [String] -> [(String,Double)]
-- sort strings from given array on tuples of string/double
-- matching months and corresponding values 
listMonthValues [] = []
listMonthValues (a:b:c:segs) = let (_,a') = break (== ' ') a 
                              in case a' of
                                 (_:a'') -> (a'',read c) : listMonthValues segs

filterValues :: String -> [(String,Double)] -> [Double]
-- select values according to a given month
filterValues _ [] = []
filterValues key ((month,val):tuples) | month == key  = val : filterValues key tuples
                                      | otherwise     = filterValues key tuples

logMes :: String -> String -> Double
-- return the total value spent on a given month
logMes key str = foldl (+) 0 (filterValues key (listMonthValues (split ';' str)))

main = do
     a <- getLine
     b <- getLine
     let return = logMes a b
     print return
