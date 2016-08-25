-- typeInference1.hs
module TypeInference1 where

functionH :: [a] -> a
functionH (x:_) = x

functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> a
functionS (x, y) = x

-- Given a type, write the function

i :: a -> a
i x = x

c :: a -> b -> a
c a b = a

c'' :: b -> a -> b
c'' b a = b

c' :: a -> b -> b
c' a b = b

r :: [a] -> [a]
r a = a ++ a

-- f :: (b -> c) g :: (a -> b)
co :: (b -> c) -> (a -> b) -> (a -> c)
co f g a = (f . g) a

-- f :: (a -> c) -> a; x :: a; y :: c 
a :: (a -> c) -> a -> a
a f g = g

a' :: (a -> b) -> a -> b
a' f b = f b

