module Natural where
import Data.List

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

extractNat :: Maybe Nat -> Nat
extractNat (Just n) = n

integerToNat :: Integer -> Maybe Nat
integerToNat n = go n
  where go n
          | n < 0 = Nothing
          | n == 0 = Just Zero
          | otherwise  = Just $ Succ ( extractNat (go (n - 1)) )

-------------------------------------------------------------------------------------------

isJust :: Maybe a -> Bool
isJust (Just a)  = True
isJust _         = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f Nothing = b
mayybee b f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing  = a
fromMaybe _ (Just b) = b

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a:as) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = concat . map (maybeToList)

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
          | (and . map isJust) xs == False = Nothing
          | otherwise = Just (catMaybes xs)

{- possible solution with "Eq a => "  -} 
{-
  | elem Nothing xs == True = Nothing
  | otherwise = Just (catMaybes xs)
-}

-------------------------------------------------------------------------------------------
{- small library for Either -}

tleft = [Left "1", Right "2", Left "3"]

getLeft :: Either a b -> Maybe a
getLeft (Left  a) = Just a
getLeft  _ = Nothing

getRight :: Either a b -> Maybe b
getRight (Right b) = Just b
getRight _ = Nothing

lefts' :: [Either a b] -> [a]
lefts' = catMaybes . (foldr ((:) . (\x -> getLeft x)) [])

rights' :: [Either a b] -> [b]
rights' = catMaybes . (foldr ((:) . (\x -> getRight x)) [])

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' xs = (lefts' xs, rights' xs) 

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right b) = Just $ f b
eitherMaybe' _ _ = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c 
either' f _ (Left a)  = f a
either' _ g (Right b) = g b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f (Left  a) = Nothing
eitherMaybe'' f (Right b) = Just $ either' (\x -> f x) (f) (Right b)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a) 

{-
  go f a
  where go :: (a -> a) -> a -> [a]
        go f' a' = (go f' (f' a')) 
-}

-- helper function for unfoldr
fstMaybe :: Maybe (a, b) -> a
fstMaybe (Just (a, b)) = a

sndMaybe :: Maybe (a, b) -> b
sndMaybe (Just (a, b)) = b

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b =  a' : (myUnfoldr f b') 
  where a' = fstMaybe (f b)
        b' = sndMaybe (f b)

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x

-------------------------------------------------------------------------------------------
data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

getMaybe :: Maybe a -> a
getMaybe (Just a) = a

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f a = case (f a) of
               Nothing        -> Leaf 
               Just (l, m, r) -> Node (unfold f l) m (unfold f r)
{-
unfold f a = Node (unfold f x) (y) (unfold f z) 
  where (x,y,z) = case getMaybe (f a) of
                    (x,y,z)      -> (x,y,z)
                    _            -> (
                    -}
          
  
treeBuild :: Integer -> BinaryTree Integer
treeBuild 0 = Leaf
treeBuild n = unfold (\b -> case b >= n of
                         True  -> Nothing
                         False -> (Just (b+1, b, b+1))) 0

  
