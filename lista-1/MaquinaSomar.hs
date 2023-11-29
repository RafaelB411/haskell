splitZero :: [Int] -> [[Int]]
-- split int array into subarrays when of the
-- occurrence of 0
splitZero [] = []
splitZero (n:ns) = let (l,ns') = break (== 0) (n:ns)
                    in l : case ns' of
                           [] -> []
                           (_:ns'') -> splitZero ns''

executeSum :: [[Int]] -> [Int]
-- sum elements of given subarrays, ending the execution
-- when of the occurrence of an empty subarray
executeSum [] = []
executeSum (list:lists) = case list of 
                          [] -> []
                          (n:ns) -> foldr (+) 0 list : executeSum lists

maquinaSomar :: [Int] -> [Int]
-- integrate executeSum with splitZero
maquinaSomar [] = []
maquinaSomar (0:nums) = executeSum (splitZero nums)
maquinaSomar (num:nums) = executeSum (splitZero (num:nums))

main = do
       lista <- getLine
       print $ maquinaSomar (read lista :: [Int])
