module BigNumber where

import Auxiliar

scanner :: String -> BigNumber
scanner "-" = error "Invalid BigNumber from string '-'"
scanner input@(x : xs)
  | not (all isDigit xs) || not (isDigit x || x == '-') = error ("Invalid BigNumber from string '" ++ input ++ "'")
  | x == '-' = (False, digits xs)
  | otherwise = (True, digits (x : xs))
  where
    digits = foldl (\x y -> x ++ [charToInt y]) []
    isDigit a = a `elem` ['0' .. '9']

output :: BigNumber -> String
output (_, []) = error "Invalid BigNumber: Empty List"
output num
  | sign num = strNum
  | otherwise = "-" ++ strNum
  where
    strNum = foldl (\x y -> x ++ show y) "" (mag num)

outputDiv :: (BigNumber, BigNumber) -> String
outputDiv (x, y) = "quotient: " ++ output x ++ ", remainder: " ++ output y

convToInt :: BigNumber -> Int
convToInt y = read (output y) :: Int

selectIndex :: [BigNumber] -> BigNumber -> BigNumber
selectIndex x y = x !! convToInt y

subBN :: BigNumber -> BigNumber -> BigNumber
subBN x y
  | sign x /= sign y = rectifyZeroSign (somaBN x (symmetric y))
  | otherwise = rectifyZeroSign (not (cmpBN x y), usub (mag x) (mag y))

somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN x y
  | sign x == sign y = rectifyZeroSign (sign x, usoma (mag x) (mag y))
  | sign x = rectifyZeroSign (subBN x (symmetric y))
  | otherwise = rectifyZeroSign (subBN y (symmetric x))

mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN x y = rectifyZeroSign (sign x == sign y, umul (mag x) (mag y))

divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
divBN x y
  | not (sign x) || not (sign y) = error "divBN does not support negative arguments"
  | otherwise = ((True, quotient), (True, remainder))
  where
    quotient = udiv (mag x) (mag y)
    remainder = usub (mag x) (umul quotient (mag y))

safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber)
safeDivBN x (_, [0]) = Nothing
safeDivBN x y = Just (divBN x y)
