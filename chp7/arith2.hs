module Arith2 where

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

main :: IO ()
main = do
  print (0 :: Int) -- 0
  print (add 1 0 ) -- 1
  print (addOne 0) -- 1
  print (addOnePF 0) -- 1
  print ((addOne . addOne) 0) -- 2
  print ((addOnePF . addOne) 0) -- 2
  print ((addOne   . addOnePF) 0) -- 2
  print ((addOnePF . addOnePF) 0) -- 2
  print (negate (addOne 0)) -- -1
  print ((negate . addOne) 0) -- -1
  print ((negate . addOne . addOne 
         . negate . addOne) 0) -- -1
    
------------------------------------------
tensDigit :: Integral a => a -> a
tensDigit = snd . flip divMod 10 . fst . flip divMod 10

foldBool :: a -> a -> Bool -> a
foldBool = error "Error: Need to implement foldBool!"

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y t
 | t == True  = x
 | t == False = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y t = case t of
  True  -> x
  False -> y

read :: Read a => String -> a
read = undefined

show :: Show a => a -> String
show = undefined
