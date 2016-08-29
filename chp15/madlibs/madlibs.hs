module Madlibs where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
           -> Adverb
           -> Noun
           -> Adjective
           -> String
madlibbin' e adv noun adj =
  e    <> " ! he said " <>
  adv  <> " as he jumped into his car " <>
  noun <> " and drove off with this "   <>
  adj  <> " wife."


madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj =
  mconcat [e, " ! he said ",
           adv, " as he jumped into his car ",
           noun, " and drove off withthis ",
           adj, " wife."]
-------------------------------------------------------------------------------------------
{- Quick Check general Associativity -}

-- for brevity
type S = String
type B = Bool

asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c =
  a <> (b <> c) == (a <> b) <> c

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a ) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-------------------------------------------------------------------------------------------

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

-------------------------------------------------------------------------------------------
data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) (Only b) = Only $ mappend a b
  mappend (Only a) _        = Only a
  mappend _       (Only b)  = Only b
  mappend _        _        = Nada
  

-------------------------------------------------------------------------------------------
{- Intermission: Exercises -}

newtype First' a =
  First' { getFirst' :: Optional a}
  deriving (Eq, Show)

instance Monoid (First' a) where
  mempty                        = First' Nada
  mappend t@(First'(Only _)) _  = t
  mappend (First' Nada) t       = t

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
    First' String
 -> First' String
 -> First' String
 -> Bool
-------------------------------------------------------------------------------------------

  
main :: IO ()
main = do
  quickCheck (monoidAssoc :: BullMappend)
  quickCheck (monoidLeftIdentity :: Bull -> Bool)
  quickCheck (monoidRightIdentity :: Bull -> Bool)

--  quickCheck (monoidAssoc :: FirstMappend)
--  quickCheck (monoidLeftIdentity :: First' String -> Bool)
--  quickCheck (monoidRightIdentity :: First' String -> Bool)

