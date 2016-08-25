module RegisteredUser where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
          | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name)
                          (AccountNumber acctNum))
          = putStrLn $ name ++ " " ++ show acctNum

---------------------------------------------------------

data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt  = Peng SouthAmerica
gentoo    = Peng Antarctica
macaroni  = Peng Antarctica
little    = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                 = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
  (galapagosPenguin p) || (antarcticPenguin p)

-----------------------------------------------
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a,_,c) (d,_,f) = ((a, d), (c, f))
-----------------------------------------------

funcZ x =
  case x + 1 == 1 of
    True  -> "Awesome"
    False -> "wut"

-----------------------------------------------
functionC x y =
  case (x > y) of
    True  -> x
    False -> y

ifEvenAdd2 n =
  case even n of
    True  -> n + 2
    False -> n

nums x =
  case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0
------------------------------------------------
data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _     = GT
codersRuleCEOsDrool _ Coder     = LT
codersRuleCEOsDrool e e'        = compare e e'

employeeRank :: (Employee -> Employee -> Ordering)
              -> Employee
              -> Employee
              -> IO ()
              
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither employee is the boss"
    LT -> (flip reportBoss) e e'

-------------------------------------------------------
dodgy :: Num a => a -> a -> a 
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

-------------------------------------------------------
myAbs :: Integer -> Integer
myAbs x
 | x < 0     = (-x)
 | otherwise = x

bloodNa :: Integer -> String
bloodNa x
 | x < 135   = "too low"
 | x > 145   = "too high"
 | otherwise = "just right"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
 | a^2 + b^2 == c^2 = "RIGHT ON"
 | otherwise        = "not right"

dogYrs :: (Num a, Ord a) => a -> a
dogYrs x
 | x <= 0    = 0
 | x <= 1    = x * 15
 | x <= 2    = x * 12
 | x <= 4    = x * 8
 | otherwise = x * 6

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
 | y >= 0.9  = 'A'
 | y >= 0.8  = 'B'
 | y >= 0.7  = 'C'
 | y >= 0.59 = 'D'
 | y <  0.59 = 'F'
 where y = x / 100

pal :: Eq a => [a] -> Bool
pal xs
 | xs == reverse xs = True
 | otherwise        = False

numbers ::(Ord a,  Num a) => a -> a
numbers x
 | x < 0  = -1
 | x == 0 = 0
 | x > 0  = 1
