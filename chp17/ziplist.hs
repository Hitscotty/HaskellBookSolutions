import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

sampleFs = (Cons (+1) (Cons (+3) Nil))
sampleXs = Cons 1 (Cons 2 (Cons 3 Nil))

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' 0 (Cons x xs) = pure x
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x Nil) = pure $ f x 
  fmap f (Cons x xs) =  Cons (f x ) (fmap f xs) 

instance Applicative List where
  pure a = Cons a Nil
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f fs) <*> xs = append (f <$> xs) (fs <*> xs)
  
newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                    in take' 3000 l
          ys' = let (ZipList' l) = ys
                    in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList'(pure a)
  (ZipList' a) <*> (ZipList' b) = ZipList' $ a <*> b

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil


