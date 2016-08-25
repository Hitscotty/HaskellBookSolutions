
module Reverse where

greeting :: String
greeting = "Yarrrrrr"

example :: String
example = "Curry is awesome!"

printSecond :: IO ()
printSecond = do
  putStrLn greeting

thirdLetter :: String -> Char
thirdLetter x = head $ drop 4 x

letterIndex :: Int -> Char
letterIndex x = head $  drop x example

rvrs :: String -> String
rvrs x = awesome ++ " " ++  is ++ " "  ++ curry
  where awesome = take 7 $ drop 9 x
        curry   = take 5 x
        is      = take 2 $ drop 6 x 
        
main :: IO()
main = print $ rvrs "Curry is awsome!"


-- InterMission: Exercises
{-
 1). Mood
 2). Blah or Woot
 3). changeMood is a function that takes a datatype and returns a data constructor when it sho     uld return a datatype
-}

data Mood = Blah | Woot deriving Show

changeMood Blah = Woot
changeMood    _ = Blah

  
