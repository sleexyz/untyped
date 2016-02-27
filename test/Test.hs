module Test ( Fail,
              Pass,
              Result,
              test,
              runTests
            ) where

import System.Console.ANSI

color :: Color -> String -> String
color c str = setSGRCode [SetColor Foreground Vivid c] ++  str ++ setSGRCode [Reset]

 -- DIY Testing suite
 -- I really just reinvented Either

type Fail = String
type Pass = String

data Result = Fail Fail | Pass Pass -- I'm overloading the type and the type constructor
instance Show Result where
  show (Fail a) = (color Red "Fail: ") ++ (color Black a)
  show (Pass b) = (color Green "Pass: ") ++ (color Black b)



test :: String -> Bool -> Result
test testName b
  | b         = Pass testName
  | otherwise = Fail testName

runTests :: [Result] -> IO()
runTests = foldl (\x y -> x >> print y) (putStrLn "Testing")
