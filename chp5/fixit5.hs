module Sing where

fstString :: [Char] -> [Char]
fstString x = x ++ " in the Rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the Rainbow"

sing = if (x > y) then fstString x else sndString y
   where x = "Singin"
         y = "Somewhere"
