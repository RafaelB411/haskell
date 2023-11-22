type Comando = String
type Valor = Int

executa :: [(Comando,Valor)] -> Int
executa [] = 0
executa [(cmd,num)] = case cmd of
                      "Soma" -> num
                      "Multiplica" -> 0
                      "Subtrai" -> -num
                      "Divide" -> 0
executa ((cmd,num):pairs) 
 | cmd == "Divide" && num == 0 = -666
 | (executa (pairs)) == -666   = -666
 | otherwise                   = case cmd of
                                 "Soma" -> num + (executa (pairs))
                                 "Multiplica" -> num * (executa (pairs))
                                 "Subtrai" -> num - (executa (pairs))
                                 "Divide" -> num `div` (executa (pairs))

main = do
    a <- getLine
    let result = executa (read a)
    print result
