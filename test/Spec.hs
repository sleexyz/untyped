import Lib
import Test as T

testEq :: Value -> Value -> Result
testEq input expected = T.test (show input) ((\(Prog value _) -> value ) (eval (Prog input defaultContext)) == expected)




main :: IO ()
main = T.runTests
  [ testEq (Var "I") (Lambda "x" (Var "x"))
  , testEq (Var "K") (Lambda "x" (Lambda "y" (Var "x")))
  , testEq (Apply (Apply  (Var "K") (Var "I")) (Var "I")) (Lambda "x" (Var "x"))
  , testEq (Apply (Apply (Apply  (Var "S") (Var "K")) (Var "K")) (Var "I")) (Lambda "x" (Var "x"))
  ]
