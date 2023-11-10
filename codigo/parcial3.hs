
esPrimo :: Int -> Bool
esPrimo x = 
    if x < 2 
        then False 
        else esPrimoRec x 2
        where
            esPrimoRec n i = 
                if i * i > n 
                then True
                else if n `mod` i == 0 
                    then False 
                    else esPrimoRec n (i + 1)

numerosEntreDosYelSiguienteValorX :: Int  -> [Int]
numerosEntreDosYelSiguienteValorX x = filter esPrimo [2..x] 







