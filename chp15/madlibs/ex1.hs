module Ex1 where

import Data.Monoid

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend mempty (Only a) = Only a
  mappend (Only a) mempty = Only a
