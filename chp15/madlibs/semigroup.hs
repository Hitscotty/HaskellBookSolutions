module Semi where

import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup (Semigroup, (<>), Sum(Sum, getSum))

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

-------------------------------------------------------------------------------------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)
  
type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool
-------------------------------------------------------------------------------------------

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc = Two String Ordering -> Two String Ordering -> Two String Ordering -> Bool

-------------------------------------------------------------------------------------------
data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three d f g) = Three (a <> d) (b <> f) (c <> g)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)
    
type ThreeAssoc = (Three String String Ordering)
               -> (Three String String Ordering)
               -> (Three String String Ordering)
               -> Bool

-------------------------------------------------------------------------------------------
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where

  (Four a b c d) <> (Four e f g h) = Four (a <> e) (b <> f) (c <> g) (d <> h)

  
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssoc = Four String String String String
              -> Four String String String String
              -> Four String String String String
              -> Bool
-------------------------------------------------------------------------------------------
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj a) <> (BoolConj b) = BoolConj (a && b)

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    elements [(BoolConj a), (BoolConj a)]
    
type ConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
-------------------------------------------------------------------------------------------
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj a <> BoolDisj b = BoolDisj (a || b)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    elements [(BoolDisj a), (BoolDisj a)]

type DisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
-------------------------------------------------------------------------------------------
data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)
 
instance Semigroup (Or a b) where
  Fst _ <> b = b
  b <> _ = b
 
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [(Fst a),(Snd b)]

type OrAssoc = Or String Ordering -> Or String Ordering -> Or String Ordering -> Bool
-------------------------------------------------------------------------------------------
newtype Combine a b = Combine { unCombine :: ( a -> b) }

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

instance Semigroup b => Semigroup (Combine a b) where
  a <> b = Combine (unCombine a <> unCombine b)
-------------------------------------------------------------------------------------------
newtype Comp a =
  Comp {unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
   a <> b = Comp (unComp a <> unComp b)
-------------------------------------------------------------------------------------------
data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  (<>) = undefined
-------------------------------------------------------------------------------------------
newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b =>
  Semigroup (AccumulateRight a b) where
  (<>) = undefined
-------------------------------------------------------------------------------------------
newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) =>
  Semigroup (AccumulateBoth a b) where
  (<>) = undefined
  
main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: ConjAssoc)
  quickCheck (semigroupAssoc :: DisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
