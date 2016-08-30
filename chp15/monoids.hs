
module Exercises where

import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, elements)
import Data.Semigroup  (Semigroup, (<>))

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) = mappend

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool
-------------------------------------------------------------------------------------------

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivialAssoc)
