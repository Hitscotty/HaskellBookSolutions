module WordNumber where

import Data.List (intersperse)


digitToWord :: Int -> String
digitToWord n =
  case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
   

findPlaceValue :: Int -> Int
findPlaceValue n
              | n < 10    = 0
              | otherwise = 1 + findPlaceValue (div n 10)
             
digits :: Int -> [Int]
digits 0 = [] 
digits n = headNumber : digits (nextNumber)
 where decimalPlaceValue = 10 ^ (findPlaceValue n)
       headNumber        = div n decimalPlaceValue
       nextNumber        = n - (decimalPlaceValue * headNumber)
       
wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
