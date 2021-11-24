import Auxiliar (BigNumber)
import BigNumber

fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n - 1) + fibRec (n - 2)

fibLista :: (Integral a) => a -> a
fibLista n = let lista = 0 : 1 : [lista !! fromIntegral (x - 1) + lista !! fromIntegral (x - 2) | x <- [2 .. n]] in lista !! fromIntegral n

fibListaInfinita :: (Integral a) => a -> a
fibListaInfinita n = listaInfinita !! fromIntegral n
  where
    listaInfinita = 0 : 1 : [listaInfinita !! fromIntegral (k - 1) + listaInfinita !! fromIntegral (k - 2) | k <- [2 ..]]

fibRecBN :: BigNumber -> BigNumber
fibRecBN (False, _) = error "Negative BigNumber on fibRecBN!"
fibRecBN (True, [0]) = scanner "0"
fibRecBN (True, [1]) = scanner "1"
fibRecBN n = fibRecBN (n `subBN` scanner "1") `somaBN` fibRecBN (n `subBN` scanner "2")

genInteirosFrom2 :: BigNumber -> [BigNumber]
genInteirosFrom2 (True, [2]) = [scanner "2"]
genInteirosFrom2 x = genInteirosFrom2 (x `subBN` scanner "1") ++ [x]

--fibListaBN n = let lista = scanner "0" : scanner "1" : [lista !! fromIntegral (x - 1) `somaBN` lista !! fromIntegral (x - 2) | x <- genInteirosFrom2 n] in lista !! fromIntegral n

--fibListaInfinitaBN :: (Integral a) => a -> a

--fibListaInfinita n = listaInfinita !! fromIntegral n
--  where
--    listaInfinita = 0 : 1 : [listaInfinita !! fromIntegral (k - 1) + listaInfinita !! fromIntegral (k - 2) | k <- [2 ..]]
