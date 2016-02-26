import Lib
import Test as T

testEq :: Value -> Value -> Result
testEq input expected = T.test (show input) (eval' input == expected)




main :: IO ()
main = T.runTests
  [ testEq icomb icomb
  , testEq scomb scomb
  , testEq kcomb kcomb
  , testEq (Apply icomb icomb)  icomb
  , testEq (Apply icomb kcomb)  icomb
  ]
