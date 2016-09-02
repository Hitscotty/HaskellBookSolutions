{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}

module Lifting where


import Test.QuickCheck
import Test.QuickCheck.Function


a = (+1) <$> read "[1]" :: [Int]

b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

c = (*2) <$> (\x -> x - 2)

d = fmap ((return '1' ++) . show) (\x -> [x, 1..3])


e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = (fmap ("123"++) $ fmap (\x -> show x :: String) ioi )
    in fmap ((*3) . (\x -> read x :: Integer)) changed
-------------------------------------------------------------------------------------------
data Two a b =
  Two a b
  deriving (Eq, Show)

data Or a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Functor (Or a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-------------------------------------------------------------------------------------------
{- Intermission: Exercises -}

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor (Identity) where
  fmap f (Identity a) = Identity (f a)

---

data Pair a = Pair a a deriving (Eq, Show)

instance Functor (Pair) where
  fmap f (Pair a b) = Pair (f a) (f b)

---

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

---

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

---

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

---

data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

---

{-

data Trivial = Trivial
-- this has a kind of * and a Functor needs a kind of * -> * 

-}
-------------------------------------------------------------------------------------------
{- Short Exercise -}

data Possible a =
    LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possible where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

{- Short Exercise 2 -}

data Sum a b =
    First' a
  | Second' b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First' a) = First' a 
  fmap f (Second' b) = Second' (f b)

-- Functor instance can not apply function to First' or Eithers Left, because it is apart
-- of the Functor structure to be lifted

{- Chapter Exercises -}

-- 1. No, its kind is *
-- 2. Yes, its kind is * -> *
-- 3. Yes
-- 4. Yes
-- 5. Yes

---

  

-------------------------------------------------------------------------------------------

functorIdentity :: (Functor f, Eq (f a)) =>
                        f a
                     -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                       (a -> b)
                    -> (b -> c)
                    -> f a
                    -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) =>
                     f a
                   -> Fun a b
                   -> Fun b c
                   -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do
  quickCheck (functorCompose' :: IntFC)

-------------------------------------------------------------------------------------------

type Nat f g = forall a. f a -> g a
