module Addition where

import Test.Hspec
import Test.QuickCheck


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
 where go n   d count
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1)


myMult :: (Eq a, Num a) => a -> a -> a
myMult n 0 = 0
myMult n m = n + myMult n (m - 1)

{- duplicates increases probability of randomness when used with sample -}

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) =>
               Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main = hspec $ do
  describe "dividedBy" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4, 2)
  describe "myMult" $ do
    it "5 multiplied by 5 is 25" $ do
      myMult 5 5 `shouldBe` 25
    it "4 multiplied by 7 is 28" $ do
      myMult 7 4 `shouldBe` 28
  describe "quickcheck test" $ do
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1 > (x :: Int)

      


