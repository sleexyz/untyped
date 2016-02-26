import Lib
import Test

testEq :: Value -> Value -> Result
testEq input expected = test (show input) (eval' input == expected)




main :: IO ()
main = runTests
  [ testEq icomb icomb
  , testEq scomb scomb
  , testEq kcomb kcomb
  , testEq (Apply icomb icomb)  icomb
  ]
