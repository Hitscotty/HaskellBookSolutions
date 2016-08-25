module Jammin where
import Data.List

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

{-
data JamJars =
  Jam Fruit Int
  deriving (Eq, Show)
-}

-- rewrite in record syntax
-- Cardinality = 4 * Int
data JamJars = Jam { jam :: Fruit
               , canned :: Int}
               deriving (Eq, Show, Ord)

row1 = Jam Peach 30
row2 = Jam Plum 20
row3 = Jam Apple 100
row4 = Jam Blackberry 50
row5 = Jam Peach 10
row6 = Jam Apple 20
allJam = [row1, row2, row3, row4, row5, row6]

totalJam :: [JamJars] -> Int
totalJam = foldr ((+) . (\x -> canned x)) 0

mostRow :: [JamJars] -> JamJars
mostRow =  foldr1 (\x y -> if (canned x > canned y) then x else y)

compareKind :: JamJars -> JamJars -> Ordering
compareKind (Jam k _) (Jam k' _) = compare k k'

sortJam :: [JamJars] -> [JamJars]
sortJam = sortBy compareKind

groupJam :: [JamJars] -> [[JamJars]]
groupJam = groupBy (==) . sortJam  

-------------------------------------------------------------------------------------------
data Fiction = Fiction deriving Show
data Nonfiction = Nonfiction deriving Show

data BookType = FictionBook Fiction
              | NonfictionBook Nonfiction
              deriving Show

type AuthorName = String
data Author = Author (AuthorName, BookType)
-------------------------------------------------------------------------------------------
data FlowerType = Gardenia
                | Daisy
                | Rose
                | Lilac
                deriving Show

type Gardener = String

{-
data Garden =
  Garden Gardener FlowerType
  deriving Show
-}

-- normal Form of Garden:
-- a * (b + c) => (a * b) + (a * c)
data Garden =  Gardener 
            |  FlowerType
            deriving Show

-------------------------------------------------------------------------------------------
data GuessWhat =
  Chickenbutt deriving (Eq, Show)

data Id a =
  MkId a deriving (Eq, Show)

data Product a b =
  Product  a b deriving (Eq, Show)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b =
  RecordProduct { pfirst :: a
                , psecond :: b }
                deriving (Eq, Show)

-------------------------------------------------------------------------------------------

data OperatingSystem =
       GnuPlusLinux
     | OpenBSDPlusNevermindjustBSDStill
     | Mac
     | Windows
     deriving (Eq, Show)

data ProgrammingLanguage =
       Haskell
     | Agda
     | Idris
     | PureScript
     deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda
                             , os = GnuPlusLinux }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindjustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer { os = x, lang = y} |
                   x <- allOperatingSystems, y <- allLanguages ]
       
-- 4.19 Function Type is Exponential 
data List a = Nil | Cons a (List a)

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a  = Node (insert' b left) a right
  | b > a  = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
  Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
  then print "yup okay!"
  else error "test failed!"
  
-------------------------------------------------------------------------------------------
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right 

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2,1,3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears"

testInorder :: IO ()
testInorder =
  if inorder testTree == [1,2,3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears"
  
testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

-------------------------------------------------------------------------------------------
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree f b Leaf = b
foldTree f b (Node left a right)  = foldTree f (f a (foldTree f b left)) right            
{- Inorder -}
-- foldTree f ( f a (foldTree f b left)) right
{- Preorder -}
{- Postorder -}

mapTree' :: (a -> b) -> BinaryTree a -> [b]
mapTree' f bt = map f (preorder bt)

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder
  
f :: a -> b
f = undefined
