-- print3.hs
module Print3 where

myGreeting :: String
-- The above line reads as: "myGreeting has the type String
myGreeting = "hello" ++ " world!"
-- could also be: "hello" ++ " " ++ "world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where secondGreeting = concat [hello, " ", world]
  
