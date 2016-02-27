import Lib
import Test as T

testEq :: Value -> Value -> Result
testEq input expected = T.test (show input) ((\(Prog value _) -> value ) (eval (Prog input defaultContext)) == expected)




main :: IO ()
main = T.runTests
  [ testEq (Var "I") (Var "I")
  , testEq (Var "K") (Var "K")
  , testEq (Var "S") (Var "S")
  , testEq (Apply (Apply  (Var "K") (Var "I")) (Var "S")) (Var "I")
  ]
