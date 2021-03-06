module Auxiliar where

type Sign = Bool

type Magnitude = [Int]

type BigNumber = (Sign, Magnitude)

sign :: BigNumber -> Sign
sign = fst

mag :: BigNumber -> Magnitude
mag = snd

symmetric :: BigNumber -> BigNumber
symmetric x = (not (sign x), mag x)

rectifyZeroSign :: BigNumber -> BigNumber
rectifyZeroSign (_, [0]) = (True, [0])
rectifyZeroSign x = x

charToInt :: Char -> Int
charToInt x = read [x] :: Int

cmpMag :: Magnitude -> Magnitude -> Bool
cmpMag x y
  | length x < length y = True
  | length x > length y = False
  | otherwise = x <= y

cmpBN :: BigNumber -> BigNumber -> Bool
cmpBN x y
  | sign x /= sign y = sign x
  | sign x = cmpMag (mag x) (mag y)
  | otherwise = cmpMag (mag y) (mag x)

rmleadzeros :: Magnitude -> Magnitude
rmleadzeros x
  | null result = [0]
  | otherwise = result
  where
    result = dropWhile (== 0) x

usomaaux :: Magnitude -> Magnitude -> Int -> Magnitude
usomaaux [] [] c = [c]
usomaaux left right c
  | res > 9 = res `mod` 10 : usomaaux newRight newLeft 1
  | otherwise = res : usomaaux newRight newLeft 0
  where
    newRight = if null right then [] else tail right
    newLeft = if null left then [] else tail left
    res
      | null left && null right = c
      | null left = head right + c
      | null right = head left + c
      | otherwise = head right + head left + c

usoma :: Magnitude -> Magnitude -> Magnitude
usoma x y = rmleadzeros (reverse (usomaaux (reverse x) (reverse y) 0))

-- assumes x is the largest
usubaux :: Magnitude -> Magnitude -> Int -> Magnitude
usubaux [] [] 0 = []
usubaux right left c
  | res < 0 = res `mod` 10 : usubaux newRight newLeft 1
  | otherwise = res : usubaux newRight newLeft 0
  where
    newRight = if null right then [] else tail right
    newLeft = if null left then [] else tail left
    res
      | null left && null right = -c
      | null left = head right - c
      | null right = -(head left + c)
      | otherwise = head right - (head left + c)

-- subtracts the smallest from the largest
usub :: Magnitude -> Magnitude -> Magnitude
usub x y
  | smaller = rmleadzeros (reverse (usubaux yr xr 0))
  | otherwise = rmleadzeros (reverse (usubaux xr yr 0))
  where
    smaller = cmpMag x y
    xr = reverse x
    yr = reverse y

umulaux :: Magnitude -> Int -> Int -> Magnitude
umulaux [] y 0 = []
umulaux [] y c = [c]
umulaux (x : xs) y c
  | res > 9 = res `mod` 10 : umulaux xs y (res `div` 10)
  | otherwise = res : umulaux xs y 0
  where
    res = x * y + c

umulterms :: Magnitude -> Magnitude -> [Magnitude]
umulterms x y = [reverse (umulaux (reverse x) (reverse y !! n) 0) ++ replicate n 0 | n <- [0 .. length y - 1]]

umul :: Magnitude -> Magnitude -> Magnitude
umul x y
  | length x < length y = rmleadzeros (foldl usoma [] (umulterms x y))
  | otherwise = rmleadzeros (foldl usoma [] (umulterms y x))

quotientcount :: Magnitude -> Magnitude -> Magnitude
quotientcount x [0] = [0]
quotientcount x div
  | cmpMag div x = usoma [1] (quotientcount (usub x div) div)
  | otherwise = [0]

udivaux :: Magnitude -> Magnitude -> Magnitude -> Magnitude
udivaux [] d q
  | cmpMag d q = quotientcount q d
  | otherwise = [0]
udivaux (x : xs) d q
  | cmpMag d q = k ++ udivaux xs d (rmleadzeros (usub q (umul k d) ++ [x]))
  | otherwise = 0 : udivaux xs d (q ++ [x])
  where
    k = quotientcount q d

udiv :: Magnitude -> Magnitude -> Magnitude
udiv x y = rmleadzeros (udivaux x y [])