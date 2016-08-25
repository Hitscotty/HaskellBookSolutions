import Control.Monad (forever)
import System.Exit (exitSuccess)
import Data.Char

sample :: String
sample = "Madam I'm Adam"

cleanString :: String -> String
cleanString line =  [toLower x | x <- (concat . words $ line), (elem x ";'.") == False]

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case ((cleanString line1) == reverse (cleanString line1)) of
    True -> putStrLn "Its a palindrome!"
    False -> do putStrLn "Nope!"
                exitSuccess

