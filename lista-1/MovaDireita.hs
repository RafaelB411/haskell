addEspacos :: Int -> String
-- return a string with n spaces
addEspacos 0 = []
addEspacos n = " " ++ addEspacos (n-1)

paraDireita :: Int -> String -> String
-- move a string to the right adding n spaces on the left
paraDireita n [] = addEspacos n
paraDireita n str = addEspacos n ++ str

parseInput str = let [n,s] = words str
                  in (read n,s)

main :: IO()
main = interact $ uncurry paraDireita . parseInput
