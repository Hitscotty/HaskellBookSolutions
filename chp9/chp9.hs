module Chp9 where

myTail :: [a] -> [a]
myTail [] = []
myTail (_:xs) = xs

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs


myHead :: [a] -> Maybe a
myHead []     = Nothing
myHead (x:_) = Just x

-----------------------------------------------
-- applies to eftBool, eftOrd, eftInt, eftChar

eft :: (Enum a, Ord a) =>  a -> a -> [a]
eft  start stop
     | start == stop = [start]
     | start > stop  = [start]
     | start < stop  = start : eft (succ start) stop
     | otherwise     = []
-----------------------------------------------
-- using takeWhile and dropWhile only

myWords ::x String -> [String]
myWords []       = []
myWords sentence = word : myWords (dropWhile (==' ') rest)
        where word = takeWhile (/=' ') sentence
              rest = dropWhile (/=' ') sentence
------------------------------------------------
