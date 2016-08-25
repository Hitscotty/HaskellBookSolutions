module Factorial where

data DividedResult =
    Result (Integer, Integer)
  | DividedByZero
  deriving Show

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)


incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times -1) n)

applyTimes :: (Eq a, Num a) =>
               a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"
----------------------------------------------------
sumToN :: (Eq a, Num a) => a -> a
sumToN n = go n 0
 where go num sum
        | num == 0 = sum
        | otherwise = go (num - 1) (sum + num)

sumToN' :: (Eq a, Num a) => a -> a
sumToN' 0 = 0
sumToN' n = n + sumToN' (n - 1)

multTwo :: Integral a => a -> a -> a
multTwo _ 0 = 0
multTwo n m = n + multTwo n (m - 1)
        
dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom = go num denom 0
 where go n d count
        | d == 0  = DividedByZero
        | n < d = Result (count, n)
        | otherwise = go (n - d) d (count + 1)

-- McCarthy 91 function
mc91 n
    | n > 100 = n - 10
    | otherwise = (mc91 . mc91) $ n + 11
