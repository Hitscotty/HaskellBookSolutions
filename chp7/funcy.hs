-- Chapter 7. More Functional Patterns
module Funcy where

myNum :: Integer
myNum = 1

myVal f = myNum


--mth x y   = \z -> x * y * z
-- a, d
-- d

addOneIfOdd n = case odd n of
  True  -> f n
  False -> n
  where f = (\n -> n + 1)
 
-- addFive x y = (if x > y then y else x) + 5
addFive = (\x y -> (if x > y then y else x) + 5) 

-- mflip f = \x -> \y -> f y x
mflip f x y = f y x
