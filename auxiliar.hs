module Auxiliar where

type BigNumber = (Bool, [Int])

sign :: BigNumber -> Bool
sign = fst

mag :: BigNumber -> [Int]
mag = snd

symmetric :: BigNumber -> BigNumber
symmetric x = (not (sign x), mag x)

charToInt :: Char -> Int
charToInt x = read [x]::Int

cmpMag :: [Int] -> [Int] -> Bool
cmpMag x y | length x < length y = True
            | length x > length y = False
            | otherwise = x <= y

cmpBN :: BigNumber -> BigNumber -> Bool
cmpBN x y | sign x /= sign y = sign x
          | sign x = cmpMag (mag x) (mag y)
          | otherwise = cmpMag (mag y) (mag x) 

rmleadzeros :: [Int] -> [Int]
rmleadzeros x | null result = [0]
              | otherwise = result
              where result = dropWhile (==0) x

usomaaux :: [Int] -> [Int] -> Int -> [Int]
usomaaux [] [] c = [1 | c == 1]
usomaaux x [] c = head x + c : tail x
usomaaux [] y c = head y + c : tail y
usomaaux (x:xs) (y:ys) c | res > 9 = res `mod` 10 : usomaaux xs ys 1
                         | otherwise = res : usomaaux xs ys 0
                         where res = x + y + c

usoma :: [Int] -> [Int] -> [Int]
usoma x y = rmleadzeros (reverse (usomaaux (reverse x) (reverse y) 0))

-- assumes x is the largest
usubaux :: [Int] -> [Int] -> Int -> [Int]
usubaux [] [] 0 = []
usubaux x [] c = head x - c : tail x
usubaux (x:xs) (y:ys) c | res < 0  = res `mod` 10 : usubaux xs ys 1
                        | otherwise = res : usubaux xs ys 0
                        where res = x - (y + c)

-- subtracts the smallest from the largest
usub :: [Int] -> [Int] -> [Int]
usub x y | smaller = rmleadzeros (reverse (usubaux yr xr 0))
         | otherwise  = rmleadzeros (reverse (usubaux xr yr 0))
         where smaller = cmpMag x y
               xr = reverse x
               yr = reverse y

umulaux :: [Int] -> Int -> Int -> [Int]
umulaux [] y 0 = []
umulaux [] y c = [c]
umulaux (x:xs) y c | res > 9 = res `mod` 10 : umulaux xs y (res `div` 10)
                   | otherwise = res : umulaux xs y 0
                   where res = x * y + c

umulterms :: [Int] -> [Int] -> [[Int]]
umulterms x y = [reverse (umulaux (reverse x) (reverse y !! n) 0) ++ replicate n 0| n <- [0..length y - 1]]

umul :: [Int] -> [Int] -> [Int]
umul x y | length x < length y = rmleadzeros (foldl usoma [] (umulterms x y))
         | otherwise = rmleadzeros (foldl usoma [] (umulterms y x))

quotientcount :: [Int] -> [Int] -> [Int]
quotientcount x [0] = [0]
quotientcount x div | cmpMag div x =  usoma [1] (quotientcount (usub x div) div)
                    | otherwise = [0]

udivaux :: [Int] -> [Int] -> [Int] -> [Int]
udivaux [] d q | cmpMag q d = [0]
               | otherwise = quotientcount q d
udivaux (x:xs) d q | cmpMag q d = 0 : udivaux xs d (q ++ [x])
                   | otherwise = k ++ udivaux xs d (rmleadzeros (usub q (umul k d) ++ [x]))
                   where k = quotientcount q d

udiv :: [Int] -> [Int] -> [Int]
udiv x y = rmleadzeros (udivaux x y [])