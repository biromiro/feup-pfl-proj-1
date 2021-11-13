import Auxiliar

scanner :: String -> BigNumber
scanner "-" = error "Invalid BigNumber"
scanner (x:xs) | x == '-' = (False, digits xs)
               | otherwise = (True, digits (x:xs))
                where digits = foldl (\x y -> x ++ [charToInt y]) []

output :: BigNumber -> String
output (_,[]) = error "Invalid BigNumber"
output num | sign num = strNum
           | otherwise = "-" ++ strNum
           where strNum = foldl (\x y -> x ++ show y) "" (mag num)


subBN :: BigNumber -> BigNumber -> BigNumber
subBN x y | sign x /= sign y = somaBN x (symmetric y)
          | otherwise = (not (cmpBN x y), usub (mag x) (mag y))

somaBN :: BigNumber -> BigNumber -> BigNumber
somaBN x y | sign x == sign y = (sign x, usoma (mag x) (mag y))
           | sign x = subBN x (symmetric y)
           | otherwise = subBN y (symmetric x)

mulBN :: BigNumber -> BigNumber -> BigNumber
mulBN x y = (sign x == sign y, umul (mag x) (mag y))


divBN :: BigNumber -> BigNumber -> (BigNumber, BigNumber)
divBN x y | not (sign x) || not (sign y) = error "divBN does not support negative arguments"
          | otherwise  = ((True, quotient), (True, remainder))
          where quotient = udiv (mag x) (mag y)
                remainder = usub (mag x) (umul quotient (mag y))

safeDivBN :: BigNumber -> BigNumber -> Maybe (BigNumber, BigNumber)
safeDivBN x (_,[0]) = Nothing
safeDivBN x y = Just (divBN x y)
