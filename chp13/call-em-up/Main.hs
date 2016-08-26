module Main where

import Hello

main =
  sayHello "GoatScreams McGee"

{- practice using IO monad -}
{-
main :: IO ()
main = do x1 <- getChar
          x2 <- getChar
          if x1 == x2
            then putStrLn "True"
            else return ()
          -}
