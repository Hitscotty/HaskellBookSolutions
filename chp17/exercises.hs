import Control.Applicative
import Data.Maybe
import Data.List (elemIndex)



f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]
g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]
m x = lookup x [(4, 10), (8, 13), (1, 9001)]

{- short exercises -}

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])


y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled =  pure (,) <*> y <*> z
