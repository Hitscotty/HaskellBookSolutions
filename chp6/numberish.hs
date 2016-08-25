-- keep your typeclass instances for a type
-- in the same file as that type
-- we'll explain why later
{-
data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True
-}

import Data.List

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Show)

  -- day of week and numerical day of month

data Date =
  Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon   = True
  (==) Tue Tue   = True
  (==) Weds Weds = True
  (==) Thu Thu   = True
  (==) Fri Fri   = True
  (==) Sat Sat   = True
  (==) Sun Sun   = True
  (==) _ _       = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
    weekday == weekday' && dayOfMonth == dayOfMonth'

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _   = GT
  compare _ Fri   = LT
  compare _ _     = EQ
  
-- Intermission: Exercises
----------------------------------------------------
data TisAnInteger =
  Tisan Integer

instance Eq TisAnInteger where
  (==) (Tisan n) (Tisan m) = n == m
----------------------------------------------------
data TwoIntegers =
  Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two n m) (Two n' m')  = n == n' && m == m'
-----------------------------------------------------
data StringOrInt =
    TisAnInt  Int
  | TisAString String

instance Eq StringOrInt where
  (==) (TisAnInt n) (TisAnInt n')     = n == n'
  (==) (TisAString m) (TisAString m') = m == m'
  (==) _ _                            = False
-----------------------------------------------------
data Pair a =
  Pair a a

instance Eq a => Eq (Pair a) where
  (==) (Pair n m) (Pair n' m') = n == n' && m == m'
-----------------------------------------------------
data EitherOr a b =
    Hello a
  | GoodBye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a')     = a == a'
  (==) (GoodBye b) (GoodBye b') = b == b'
  (==) _ _                      = False
-----------------------------------------------------
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Blah
          | Woot deriving (Show, Eq)

-- settleDown :: Mood -> Mood
settleDown x = if x == Woot
                  then Blah
                  else x
------------------------------------------------------
type Subject = String
type Verb    = String
type Object  = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
------------------------------------------------------
data Rocks =
  Rocks String deriving (Ord, Eq,  Show)

data Yeah =
  Yeah Bool deriving (Ord, Eq,  Show)

data Papu =
  Papu Rocks Yeah
  deriving (Ord, Eq,  Show)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p'

comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'
------------------------------------------------------
i :: Num a => a
i = 1

f :: RealFrac a => a
f = 1.0

freud :: Ord a =>  a -> a
freud x = x

freud' :: Int -> Int
freud' x = x

myX = 1 :: Int

sigmund :: Num a => a -> Int
sigmund x = myX

--jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs)

mySort ::Ord a =>  [a] -> [a]
mySort = sort

--signifier :: [Char] -> Char
signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs)
-------------------------------------
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f b a = (+) (f a) (fromInteger b)    



