-- unzip :: [(a,b)] -> ([a],[b])
-- unzip [] = ([],[])
-- unzip ((a,b):xs) = (a:as, b:bs)
--     where (as,bs) = unzip xs

unzip' :: [(a,b)] -> ([a],[b])
-- unzip clone function using foldr
unzip' pairs = foldr auxUnzip' ([],[]) pairs
    where auxUnzip' (a,b) ([],[]) = ([a],[b])
          auxUnzip' (a,b) (as,bs) = (a:as,b:bs)

main = interact $ show . unzip' . (read :: String -> [(Int, Int)])