{- Chapter 9 Exercises -}

module Cipher where

import Data.Char
import Data.List
import Control.Monad

{- Ciphers -}

shift :: Char -> Int -> Char
shift c n = chr $ (mod shifted 26) + 64
  where
    wrap    = (ord (toUpper c)) - 64
    shifted = wrap + n

caeser :: String -> Int -> String
caeser c n = map (\x -> shift x n) c

uncaeser :: String -> Int -> String
uncaeser c n = caeser c (-n)

{- Vigenere -}
vig :: String -> String -> String
vig k p = zipWith (\x y -> shift x y) text keys
  where text  = [ toUpper x | x <- p ]
        keys  =  map (\x -> (ord x) - 65) [ toUpper y | y <- k ]

vigSent :: String -> String -> [String]
vigSent p k =  map (vig k) (words p) 

-- keys = cycle $ map (\x -> (ord x) - 65) 
-- [ zipWith (\x y -> shift x y) x y | x <- words p, y <- keys
vigenere :: String -> String -> String
vigenere p k = zipWith (\x y -> if (x /=' ') then shift x y else ' ') text keys
  where text  = [ toUpper x | x <- p ]
        keys  =  cycle $ map (\x -> (ord x) - 65) [ toUpper y | y <- k ]
{- Writing your own standard Functions -} 

-- returns True if any bool in the list is True
{- 1 -}
myOr :: [Bool] -> Bool
myOr [] = False
myOr (True:_) = True 
myOr (_:xs)   = myOr xs
 
{- 2 -}
-- returns True if a -> Bool applied to any of the valies in the list
-- returns True
myAny :: (a -> Bool) -> [a] -> Bool
myAny f a = myOr $  map f a

{- 3 -}
-- recursive myElem
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = myOr $ (x == a) : myElem a xs : []

myElem' :: Eq a => a -> [a] -> Bool
myElem' x xs  = myAny (==x) xs


{- 4 -}
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]      

{- 5 -}
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

{- 6 -}
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f a = squish $ map f a

{- 7 -}
squishAgain :: [[a]] -> [a]
squishAgain = squishMap (\x -> id x) 

{- 8 -}
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs) =
  let
    y = myMaximumBy f xs
  in
    case f x y of
      LT -> y
      EQ -> x
      GT -> x

{- 9 -}
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ (x:[]) = x
myMinimumBy f (x:xs) =
  let
    y = myMinimumBy f xs
  in
    case f x y of
      LT -> x
      EQ -> x
      GT -> y

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare


caeserCrypt :: IO String
caeserCrypt = do
  putStrLn "Enter text to encrypt: "
  plainText <- getLine
  putStrLn "Enter Caeser Shift: "
  shiftBy <- getLine 
  return $ caeser plainText (read shiftBy :: Int)
  
vigCrypt :: IO String
vigCrypt = do
  putStrLn "Enter text to encrypt: "
  plainText <- getLine
  putStrLn "Enter shifting key: "
  shiftBy <- getLine
  return $ vigenere plainText shiftBy

main :: IO String
main = do
  putStrLn "caesar or vigenere (c / v):"
  choice <- getLine
  if choice  == "c"
  then caeserCrypt
  else vigCrypt
