type Comando = String
type Valor = Int

invertPairs :: [(Comando,Valor)] -> [(Comando,Valor)]
invertPairs [] = []
invertPairs (pair:pairs) = invertPairs pairs ++ [pair]

operador :: [(Comando,Valor)] -> Int
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
executa [] = 0
executa instruc = operador (invertPairs instruc)

main = do
    a <- getLine
    let result = executa (read a)
    print result
