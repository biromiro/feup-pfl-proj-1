fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n-1) + fibRec (n-2)

fibLista :: (Integral a) => a -> a
fibLista n = let lista = 0 : 1 : [lista !! fromIntegral (x-1) + lista !! fromIntegral (x-2) |  x <- [2..n]] in lista !! fromIntegral n

fibListaInfinita :: (Integral a) => a -> a
fibListaInfinita n = listaInfinita !! fromIntegral n
                    where listaInfinita = 0 : 1 : [listaInfinita !! fromIntegral (k-1) + listaInfinita !! fromIntegral (k-2) | k <- [2..]]