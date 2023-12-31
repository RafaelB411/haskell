data Tree t = Node t (Tree t) (Tree t)
            | Nilt
            deriving (Read, Show)

convertBases :: Int -> String
-- convert numerical code into base indicative letters
convertBases 0 = "E"
convertBases 1 = "M"
convertBases 2 = "A"
convertBases 3 = "C"
convertBases 4 = "S"

treeToStr :: Tree Int -> String
-- turn a tree into a string using 'convertBases'
treeToStr Nilt = []
treeToStr (Node num subtree1 subtree2) = treeToStr subtree1 ++ convertBases (mod num 5) ++ treeToStr subtree2

breakStr :: String -> String -> Int -> [String]
-- split a string into 8-characters sized substrings
breakStr substr [] len = [substr]
breakStr substr (a:as) 8 = substr : breakStr [] (a:as) 0
breakStr substr (a:as) len = breakStr (substr ++ [a]) as (len+1)

dna1 :: Tree Int -> [String]
-- combine 'breakStr' and 'treeToStr' to convert 
-- a tree into a list of strings 
dna1 Nilt = []
dna1 (Node num subtree1 subtree2) = breakStr [] (treeToStr (Node num subtree1 subtree2)) 0

main :: IO ()
main = do
       input <- getLine
       let result = dna1 (read input :: Tree Int)
       print result