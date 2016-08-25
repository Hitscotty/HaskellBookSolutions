module Phone where

import Data.Char
import Data.List

-- validButtons = "1234567890*#"
-- Valid presses : 1 and up

type Digit = Char 
type Presses = Int

data Button = Button Digit String deriving Show
data Phone = Phone [Button] deriving Show

{-- Accessor Functions -} 

getDigit :: Button -> Digit
getDigit (Button d _) = d

getString :: Button -> String
getString (Button _ s) = s


getButton :: Phone -> Char -> Button
getButton (Phone []) c     = Button '/' "none"
getButton (Phone (x:xs)) c = btn
  where
    btn
        | elem c (getString x) == True = x
        | otherwise = getButton (Phone xs) c

getPresses :: Phone -> Digit -> Presses
getPresses p d = index (getString (getButton p d))  d
-------------------------------------------------------------------------------------------

{- Test data -}
convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do i think I am pretty lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

nokia :: Phone
nokia = Phone
  [Button '1' "",     Button '2' "abc", Button '3' "def",
   Button '4' "ghi",  Button '5' "jkl", Button '6' "mno",
   Button '7' "pqrs", Button '8' "tuv", Button '9' "wxyz",
   Button '*' "",     Button '0' " ",   Button '#' ".,"
  ]

-------------------------------------------------------------------------------------------

{- Phone exercise functions -}   
index :: String -> Digit -> Presses
index s d = (+1) . length . takeWhile (/=d) $ s

reverseTaps :: Phone -> Char -> [(Digit, Presses)]
reverseTaps p c = ifCapital ++ [(key, press)] 
  where
    key = getDigit $ getButton p (toLower c)
    press = getPresses p (toLower c)
    ifCapital
      | isUpper c == False = []
      | otherwise = [('*', 1)]

cellPhonesDead :: Phone -> String -> [(Digit, Presses)]
cellPhonesDead p s = concat $  map (\x -> reverseTaps p x) s

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr ((+) . snd) 0

mostPopularLetter :: String -> Char
mostPopularLetter = undefined 

coolestLtr :: [String] -> Char
coolestLtr = undefined

coolestWord :: [String] -> String
coolestWord = undefined

                      {- trial and tribulations -}
-- foldr (max . getPresses nokia) 0 (cellPhonesDead nokia s)
-- max (fingerTaps . (reverseTaps nokia) x) (mostPopularLetter xs)

hello_worlds n = map (putStrLn . read) $ replicate n "Hello World"
