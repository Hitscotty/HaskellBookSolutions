import Data.Monoid

xs = [1..3]
ys = [4..6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> (pure (,) <*> x <*> y)

