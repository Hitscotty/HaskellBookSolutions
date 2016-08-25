module EqCaseGuard where

import Data.List

type Name = String
type Age  = Integer
type ValidatePerson a = Either [PersonInvalid] a


data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving Eq

-- compiles fine without Eq
toString :: PersonInvalid -> String
toString NameEmpty = "NameEmpty"
toString AgeTooLow = "AgeTooLow"

instance Show PersonInvalid where
  show = toString

-- this does not work without an Eq instance
blah :: PersonInvalid -> String
blah pi
  | pi == NameEmpty = "NameEmpty"
  | pi == AgeTooLow = "AgeTooLow"
  | otherwise = "???"

-------------------------------------------------------------------------------------------

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise = Left AgeTooLow

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
  True  -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True  -> Right name
  False -> Left [NameEmpty]

mkPerson2 :: Name -> Age -> ValidatePerson Person
mkPerson2 name age = mkPerson2' (nameOkay name) (ageOkay age)

mkPerson2' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson2' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson2' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson2' (Left badName) _             = Left badName
mkPerson2' _              (Left badAge) = Left badAge

-------------------------------------------------------------------------------------------
-- a: *, f: * -> *  
r :: a -> f a
r = undefined

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a     = Just a

replaceThe :: String -> Maybe String
replaceThe [] = Nothing  
replaceThe s = Just $ (concat . intersperse " " . f . words) s
  where f = map (\x -> if x == "the" then "a"  else x)

extract :: Maybe String -> String
extract Nothing = ""
extract (Just a) = a

replaceThe' :: String -> String
replaceThe' = extract . replaceThe

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel [] = 0
countTheBeforeVowel s  = go (words s)
  where
    go (x:xs)
       | x == "the" && elem ((head . head) xs) "aeiou" = 1 + go xs
       | xs == [] = 0
       | otherwise = go xs

countVowels :: String -> Integer
countVowels = fromIntegral . length . (filter (\x -> elem x "aeiou")) 
{- possible definitions -}
  --fromIntegral $ length [x | x <- s, elem x "aeiou"] 

{- Vlidate the word -}
-------------------------------------------------------------------------------------------
newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord s
    | s == "" = Nothing 
    | vowel > consonant = Just (Word' "Not Real")
    | otherwise = Just (Word' s)
  where
    vowel = countVowels s
    consonant = (fromIntegral (length s)) - vowel
