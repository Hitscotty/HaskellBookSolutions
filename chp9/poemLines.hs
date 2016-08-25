
module PoemLines where

import Data.Bool
import Data.Char

firstSen  = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen  = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
         ++ thirdSen ++ fourthSen

myWords' :: String -> Char -> [String]
myWords' [] _           = []
myWords' sentence regex = word : myLines (dropWhile (==regex) restOfWord)
  where word       = takeWhile (/=regex) sentence
        restOfWord = dropWhile (/=regex) sentence
  
myLines :: String -> [String]
myLines []       = []
myLines sentence = word : myLines (dropWhile (=='\n') restOfWord)
  where word       = takeWhile (/='\n') sentence
        restOfWord = dropWhile (/='\n') sentence

myLines' :: String -> [String]
myLines' sentence = myWords' sentence '\n'   

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

-- Notes 
mySqr  = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

myTuples = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

----------------------------------------------
-- Intermission: Exercises
-- [1, 2, 3, 4, 5] Neither
-- 1 : 2 : 3 : 4 : _ WHNF
-- enumFromTo 1 10 Neither
-- length [1, 2, 3, 4, 5] Neither
-- sum (enumFromTo 1 10) Neither
-- ['a' .. 'm'] ++ ['n' .. 'z'] Neither
-- (_,'b') WHNF
----------------------------------------------
{- 4 -}
itIsMystery :: String  -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs

{- 6 -}
mapBool :: (Eq a, Num a) =>  [a] -> [a]
mapBool xs = map (\x -> bool x (negate x) (x == 3)) xs   
--------------------------------------------------------

filter' :: (Eq a, Integral a) => [a] -> [a]
filter' xs = [x | x <- xs, (rem x 3) == 0]  

filter'' :: (Eq a, Integral a) => [a] -> [a]
filter'' xs = filter (\x -> (rem x 3) == 0) xs

multOf3 :: Integral a  => [a] -> Int
multOf3 xs = length . filter' $ xs
        
myFilter :: String -> [String]
myFilter xs = filter (\x -> x /= "the" && x /="a" && x /="an") (words xs) 

myFilter' :: String -> [String]
myFilter' xs = [x | x <- (words xs) , x /= "the" && x /= "a" && x /= "an"] 
---------------------------------------------------------------------------
{- 1 -}
myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys
{- 2 -}
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f a b = map (\(x,y) -> f x y) (myZip a b) 

myZipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith' f _ [] = []
myZipWith' f [] _ = []
myZipWith' f (x:xs) (y:ys) = f x y : myZipWith f xs ys  
---------------------------------------------------------------------------
{- Chapter Exercises -}
{- 2 -}
wordToUpper :: String -> String
wordToUpper  = map toUpper 

{- 3 -}
firstUpper :: String -> String
firstUpper (x:xs) = toUpper x : xs  

{- 4 -}
newUpper :: String -> String
newUpper [] = []
newUpper (x:xs) = toUpper x : newUpper xs

{- 5 -}
firstLetter :: String -> Char
firstLetter = toUpper . head 

firstLetter' :: String -> Char
firstLetter' = head . firstUpper

main :: IO ()
main = do
  print $ "Are they equal? "
          ++ show (myLines sentences == shouldEqual)
  print $ "Are they equal? "
          ++ show (myLines' sentences == shouldEqual)
