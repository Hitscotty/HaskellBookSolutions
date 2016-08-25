import Data.Char
import Data.List

data Doggies a =
    Husky a
  | Mastif a
  deriving (Eq, Show)

-- Intermission: Exercises
{-
Doggies is a type constructor
the kind of Doggies is * -> *
the kind of Doggies String is *
the type of Husky 10 is Doggies String
the type of Husky (10 :: Integer) is Doggies Integer
the type of Mastiff "Scooby Doo" is Doggies String
DogueDeBordeaux is of type  a -> DogueDeBordeaux a
the type of DogueDeBordeaux "doggie!" is DogueDeBordeaux String
-}

data Size =
  Size Integer deriving (Eq, Show)
data Price =
  Price Integer deriving (Eq, Show)

data Manufacturer =
                 Mini
               | Mazda
               | Tata
                 deriving (Eq, Show)

data Airline =
            PapuAir
          | CatapultsR'Us
          | TakeYourChansesUnited
            deriving (Eq, Show)

data Vehicle = Car Manufacturer Price
             | Plane Airline Size
               deriving (Eq, Show)

myCar    = Car Mini (Price 14000)
urCar    = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge     = Plane PapuAir (Size 88)

isCar :: Vehicle -> Bool
isCar (Car _ _ ) = True
isCar _          = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = map isCar 

getManu :: Vehicle -> Manufacturer
getManu (Car x _) = x 

------------------------------------------------------------------------------------------

class TooMany a where
  tooMany :: a -> Bool
  
instance TooMany Int where
  tooMany n = n > 42

-------------------------------------------------------------------------------------------
newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43

-------------------------------------------------------------------------------------------

newtype Tup = Tup (Int, String) deriving Show

instance TooMany Tup where
  tooMany (Tup (n,m)) = m == "hello" 

-------------------------------------------------------------------------------------------

newtype Fields = Fields (Goats, Goats) deriving (Show)

instance TooMany Fields where
  tooMany (Fields (Goats n, Goats m)) = (n + m) > 42

-------------------------------------------------------------------------------------------

data Person =
  Person { name :: String
         , age :: Int }
         deriving (Eq, Show)
-------------------------------------------------------------------------------------------
f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

capitalizeWords' :: String -> [(String, String)]
capitalizeWords' t@(_:xs) = undefined
                             
isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf'  _ []  = False
isSubsequenceOf' t@(x:xs) (y:ys)  = elem x t || (isSubsequenceOf' t xs)

capitalizeWords :: String -> String
capitalizeWords t@(x:xs) = concat . intersperse " " $ map (capitalizeWord) (words t) 

capitalizeWord :: String -> String
capitalizeWord (' ':xs) = capitalizeWord xs
capitalizeWord ('.':xs) = capitalizeWord xs
capitalizeWord (x:xs)   =  toUpper x : xs

{- incorrect, but alas we move forward -}
capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph p = first ++ capitalizeParagraph next
  where first = capitalizeWord $ takeWhile (/='.') p
        next  = drop ((length first) + 1) p 

sample = "blah. woot ha."

--fix = capitalizeWord . takeWhile (/='.') : drop 

test = capitalizeParagraph sample

main :: IO ()
main = do
  if capitalizeParagraph sample == "Blah. Woot ha."
  then print "Test Passed!"
  else print "Test Failed!"
