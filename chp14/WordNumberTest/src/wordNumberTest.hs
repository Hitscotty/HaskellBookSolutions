module WordNumberTest where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck


{- To Test with hspec -}
{-
--import WordNumber (digitToWord, digits, wordNumber)

main :: IO ()
main = hspec $ do
  describe "digitToWord does what we want" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits does what we want" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber does what we want" $ do
    it "returns one-zero-zero-one for 9001" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldbe` "nine-zero-zero-one"
 -}

-- Using QuickCheck

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t)  = (Just y, x >= y)

plusAssociative :: Int -> Int -> Int -> Bool
plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative :: Int -> Int -> Bool
plusCommutative x y =
  x + y == y + x

multAssociative :: Int -> Int -> Int -> Bool
multAssociative x y z =
  x * (y * z) == (x * y) * z
  
multCommutative :: Int -> Int -> Bool
multCommutative x y =
  x * y == y * x

idQuotRem :: Int -> Int -> Bool
idQuotRem x y =
  (quot x y ) * y + (rem x y) == x

idDivMod :: Int -> Int -> Bool
idDivMod x y =
  (div x y) * y + (mod x y) == x

powAssociative :: Int -> Int -> Int -> Bool
powAssociative z y x =
  x ^ (y ^ z) == (x ^ y) ^ z

powCommutative :: Int -> Int -> Int -> Bool
powCommutative z y x =
  x ^ y ^ z == z ^ y ^ x

-- experimate on polymorphic tests --
-- these didnt work, but fun to try
fAssociative f x y z =
   f (f x y) z == f x (f y z) 

reverseId :: [Int] -> Bool
reverseId xs =
  (reverse . reverse) xs == id xs

propertyOfComp :: Int -> Bool
propertyOfComp x =
  (f $ g x) == ((f . g) x)
  where f = (+1)
        g = (*1)


foldEqual :: [Int] -> Bool
foldEqual x =
  foldr (:) [] x == (++) [] x

isThatSo :: Int -> [Int] -> Bool
isThatSo x xs =
  length (take x xs) == x

readShow :: Int -> Bool
readShow a =
  read (show a) == a

square :: Int -> Bool
square =
  (x * x) == (x ^ x)
  
squareIdentity :: Int -> Bool
squareIdentity = undefined

runQc :: IO ()
runQc = do
  putStrLn "plus is Assiociative..."
  quickCheck plusAssociative
  putStrLn "plus is Commutative..."
  quickCheck plusCommutative

  putStrLn "mult is Assiociative..."
  quickCheck multAssociative
  putStrLn "mult is Commutative..."
  quickCheck multCommutative

  putStrLn "idQuot..."
  quickCheck idQuotRem
  putStrLn "idRem..."
  quickCheck idDivMod

  putStrLn "pow is Commutative"
  quickCheck powCommutative
  putStrLn "pow is Associative"
  quickCheck powAssociative

  putStrLn "revere equal to reverse . reverse"
  quickCheck reverseId

  putStrLn " f $ g equals f . g "
  quickCheck propertyOfComp
  
  putStrLn "fold Equal to (++)..."
  quickCheck foldEqual

  putStrLn "is that so?"
  quickCheck isThatSo

 
main :: IO ()
main = runQc
