
{- Intermission: Exercises -}

-- 
{- 3 -}
-- one difference between foldr and fold is: foldr, but not foldl, associated to the right

{- 4 -}
-- catamorphisms, are generally used to reduce structure

{- 5 -}
-- a) foldr (++) "WOOT" ["woot", "WOOT", "woot"] 
-- b) foldr max 'z' "fear is the little death"
-- c) foldr (&&) True [False, True]
-- d) this can never return a different answer
-- e) foldl (++) "" (map show [1..5])

-- f) foldr (flip const) 'a' [1..5]
-- using flips fixes:
-- must be flipped due to type error; 'a' binds foldr to (a -> Char -> a)
-- after first fold (const 1 ( const 2( const 3( const 4( const 5 'a')))))
-- becomes (a -> Num -> a) when it should be Char

-- g) foldr (flip const) 0 "tacos"
-- h) foldl const 0 "burritos"
-- i) foldl const 'z' [1..5]

module Chp10 where

import Data.Time
import Prelude hiding (Word)

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbNumber 9002
  , DbString "Hello, World!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- how to get values of types out of data structures >> 
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = [t | (DbDate t) <- db]

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = [n | (DbNumber n) <- db]

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate 

sumDB :: [DatabaseItem] -> Integer
sumDB = sum . filterDbNumber

avgDB :: [DatabaseItem] -> Double
avgDB db = fromIntegral summed / fromIntegral total
  where summed = sumDB db
        total  = length $ filterDbNumber db 
-----------------------------------------------------
fibs = take 20 $ 1 : scanl (+) 1 fibs 
-- fibs = filter (<100) $ 1 : scanl (+) 1 fibs 

fibsN x = fibs !! x
-----------------------------------------------------
type Letter    = Char
type Paragraph = [Sentences]
type Sentences = [(Word, Word, Word)]
type List      = [Word] 
type Word      = (Letter, Letter, Letter)

stops  = "pbtdkg"
vowels = "aeiou"
sent   = vowelComb stops vowels

-- comb is an all purpose function for finding combinations of a's in a 3 - tuple 
comb :: [a] -> [a] -> [(a,a,a)]
comb s v  = combine (reverse s) v s ++ combine s v s ++ combine s v (reverse s)
  where combine f s t = [ (x,y,z) | x <- f, y <- s, z <- t]

vowelComb :: String -> String -> [(Letter, Letter, Letter)]
vowelComb s v  = comb s v

-- by creating new data types i can abstract out complexity and use the all purpose comb
-- to create a solution
combSent :: List -> List -> Sentences
combSent xs ys = comb xs ys 
-------------------------------------------------------------------------------------------
seekritFunc :: String -> Double
seekritFunc x =
  (/) (fromIntegral (sum (map length (words x))))
      (fromIntegral (length (words x)))
-------------------------------------------------------------------------------------------
debBools = [False, False, False, True]

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr ((||) . (==a)) False 

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\a b -> if f a == True then a : b  else b) []

squish :: [[a]] -> [a]
squish = foldr (flip $ foldr (:)) []

squishMap :: (a -> [b]) -> [a] -> [b]

