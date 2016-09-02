import Data.List (elemIndex)

x :: Maybe Int
x = elemIndex 3 [1..5]

y :: Maybe Int
y = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = pure max' <*> x <*> y

