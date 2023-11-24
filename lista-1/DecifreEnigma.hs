traduz :: Char -> [(Char,Char)] -> Char
-- translates a char into its relative char 
-- according to a given cipher
traduz a [] = a
traduz a ((key,chv):cipher) | a == key  = chv
                            | otherwise = traduz a (cipher)

decEnigma :: String -> [(Char,Char)] -> String
-- translates a string according to a given cipher
-- using char translation function
decEnigma [] _ = []
decEnigma (a:as) cipher = traduz a cipher : decEnigma (as) cipher

main = do
    a <- getLine
    b <- getLine
    let result = decEnigma a (read b)
    print result
