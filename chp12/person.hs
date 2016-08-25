module Person where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /="" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Please enter your name: "
  name <- getLine
  putStrLn "Please enter your age: "
  age <- getLine
  case mkPerson name (read age :: Integer) of
    (Right (Person _ _)) -> putStrLn "yay"
    (Left s)             -> putStrLn (show s :: String)
    (_)                  -> putStrLn "please enter a name and age only!"
