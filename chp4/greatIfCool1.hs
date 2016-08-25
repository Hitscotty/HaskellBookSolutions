-- greatIfCool1.hs
module GreatIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
    then putStrLn "eyyyyy, What's shaking'?"
  else
    putStrLn "pshhhh."
  where cool = coolness == "downright frosty yo"


-- length :: [a] -> Int

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x > 0 then x  else negate x  

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d)  = ( ((snd (a, b)), snd (c, d)), (fst (a, b), fst (c, d)) )

-- Reading Syntax

x = (+)

f' xs = w `x` 1
   where w = length xs

id1 = \x -> x 

idFixed = \(x:xs) -> x

f4 (a, b) = a

-- chapter 5

addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

funcIgnoresArgs :: a -> a -> a -> String
funcIgnoresArgs x y z = "Blah"

