import Lib

main :: IO ()
main = putStrLn "Test suite not yet implemented"



testFrom :: Value -> Value -> Result
testFrom input expected = test (show input) (input == expected)

-- DIY Testing suite

type Fail = String
type Pass = String

data Result = Fail Fail | Pass Pass -- I'm overloading the type and the type constructor
instance Show Result where
  show (Fail a) = "Fail: " ++ a
  show (Pass b) = "Pass: " ++ b



test :: String -> Bool -> Result
test testName b
  | b         = Pass testName
  | otherwise = Fail testName

runTests :: [Result] -> IO()
-- This is how you mapM_ without mapM
runTests = foldl (\x y -> x >> print y) (putStrLn "Testing")



main :: IO ()
main = runTests
  [ testFrom icomb icomb
  ]
