type Comando = String
type Valor = Int

invertPairs :: [(Comando,Valor)] -> [(Comando,Valor)]
-- reverses the order of the given instructions 
-- so that operations are executed on the right order
invertPairs [] = []
invertPairs (pair:pairs) = invertPairs pairs ++ [pair]

operador :: [(Comando,Valor)] -> Int
-- executes given instructions 
operador [(cmd,num)] = case cmd of
                       "Soma" -> num
                       "Multiplica" -> 0
                       "Subtrai" -> -num
                       "Divide" -> 0
operador ((cmd,num):pairs)
 | cmd == "Divide" && num == 0 = -666
 | (operador (pairs)) == -666   = -666
 | otherwise                   = case cmd of
                                 "Soma" -> operador (pairs) + num
                                 "Multiplica" -> operador (pairs) * num
                                 "Subtrai" -> operador (pairs) - num
                                 "Divide" -> operador (pairs) `div` num

executa :: [(Comando,Valor)] -> Int
-- calls the instructions executing function while passing
-- the right-ordered array of instructions as parameter
executa [] = 0
executa instruc = operador (invertPairs instruc)

main = do
    a <- getLine
    let result = executa (read a)
    print result
