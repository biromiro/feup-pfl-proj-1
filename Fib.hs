import Auxiliar (BigNumber)
import BigNumber

fibRec :: (Integral a) => a -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n - 1) + fibRec (n - 2)

fibRecBN :: BigNumber -> BigNumber
fibRecBN (False, _) = error "Negative BigNumber on fibRecBN!"
fibRecBN (True, [0]) = scanner "0"
fibRecBN (True, [1]) = scanner "1"
fibRecBN n = fibRecBN (n `subBN` scanner "1") `somaBN` fibRecBN (n `subBN` scanner "2")

fibLista :: (Integral a) => a -> a
fibLista 0 = 0
fibLista 1 = 1
fibLista n =
  let lista = 0 : 1 : [lista !! fromIntegral (x - 1) + lista !! fromIntegral (x - 2) | x <- [2 .. n]]
   in last lista

genInteirosFrom2 :: BigNumber -> [BigNumber]
genInteirosFrom2 x = init (take (convToInt x) (iterate (\x -> x `somaBN` scanner "1") (scanner "2")))

fibListaBN :: BigNumber -> BigNumber
fibListaBN n =
  let lista =
        scanner "0" :
        scanner "1" :
          [ (lista `selectIndex` (x `subBN` scanner "1"))
              `somaBN` (lista `selectIndex` (x `subBN` scanner "2"))
            | x <- genInteirosFrom2 n
          ]
   in last lista

fibListaInfinita :: (Integral a) => a -> a
fibListaInfinita n = listaInfinita !! fromIntegral n
  where
    listaInfinita = map fst (iterate (\(x, y) -> (y, x + y)) (0, 1))

fibListaInfinitaBN :: BigNumber -> BigNumber
fibListaInfinitaBN n = listaInfinita `selectIndex` n
  where
    listaInfinita = map fst (iterate (\(x, y) -> (y, x `somaBN` y)) (scanner "0", scanner "1"))