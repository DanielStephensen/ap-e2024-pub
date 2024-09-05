module APL.Eval_Tests (tests) where


import APL.AST (Exp (..))
import APL.Eval (Val (..), eval, envEmpty)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool, (@?=))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [testCstInt0, testAdd0, testSub0, testMul0, testDiv0, testDiv1, testPow0, testPow1, testEql0, testEql1, testEql2, testEql3, testEql4, testEql4, testIf0, testIf1, testIf2, testIf3, testIf4, testLet0, testLet1]

testCstInt0 :: TestTree
testCstInt0 = testCase "should work" $ eval envEmpty (CstInt 5) @?= Right (ValInt 5)

testAdd0 :: TestTree
testAdd0 = testCase "should work" $ eval envEmpty (Add (CstInt 5) (CstInt 4)) @?= Right (ValInt 9)

testSub0 :: TestTree
testSub0 = testCase "should work" $ eval envEmpty (Sub (CstInt 5) (CstInt 4)) @?= Right (ValInt 1)

testMul0 :: TestTree
testMul0 = testCase "should work" $ eval envEmpty (Mul (CstInt 5) (CstInt 4)) @?= Right (ValInt 20)

testDiv0 :: TestTree
testDiv0 = testCase "should work" $ eval envEmpty (Div (CstInt 8) (CstInt 4)) @?= Right (ValInt 2)

testDiv1 :: TestTree
testDiv1 = testCase "should work" $ eval envEmpty (Div (CstInt 8) (CstInt 0)) @?= Left "Error: Division by zero is not supported"

testPow0 :: TestTree
testPow0 = testCase "should work" $ eval envEmpty (Pow (CstInt 2) (CstInt 4)) @?= Right (ValInt 16)

testPow1 :: TestTree
testPow1 = testCase "should work" $ eval envEmpty (Pow (CstInt 2) (CstInt (-4))) @?= Left "Error: Negative exponents are not supported"

testEql0 :: TestTree
testEql0 = testCase "should work" $ eval envEmpty (Eql (CstInt 2) (CstInt 1)) @?= Right (ValBool False)

testEql1 :: TestTree
testEql1 = testCase "should work" $ eval envEmpty (Eql (CstInt 2) (CstInt 2)) @?= Right (ValBool True)

testEql2 :: TestTree
testEql2 = testCase "should work" $ eval envEmpty (Eql (CstBool True) (CstBool False)) @?= Right (ValBool False)

testEql3 :: TestTree
testEql3 = testCase "should work" $ eval envEmpty (Eql (CstBool True) (CstBool True)) @?= Right (ValBool True)

testEql4 :: TestTree
testEql4 = testCase "should work" $ eval envEmpty (Eql (CstBool False) (CstBool False)) @?= Right (ValBool True)

testEql5 :: TestTree
testEql5 = testCase "should work" $ eval envEmpty (Eql (CstBool False) (CstInt 1)) @?= Left "Error: Cannot compare x and y since they have different types"

testIf0 :: TestTree
testIf0 = testCase "should work" $ eval envEmpty (If (CstBool True) (CstInt 2) (CstInt 1)) @?= Right (ValInt 2)

testIf1 :: TestTree
testIf1 = testCase "should work" $ eval envEmpty (If (CstBool False) (CstInt 2) (CstInt 1)) @?= Right (ValInt 1)

testIf2 :: TestTree
testIf2 = testCase "should work" $ eval envEmpty (If (CstBool True) (Eql (CstBool False) (CstBool False)) (CstInt 1)) @?= Right (ValBool True)

testIf3 :: TestTree
testIf3 = testCase "should work" $ eval envEmpty (If (CstBool True) (Eql (CstBool False) (CstInt 1)) (CstInt 1)) @?= Left "Error: Cannot compare x and y since they have different types"

testIf4 :: TestTree
testIf4 = testCase "should work" $ eval envEmpty (If (CstInt 1) (CstInt 2) (CstInt 1)) @?= Left "Error: Condition is not a boolean"

testLet0 :: TestTree
testLet0 = testCase "should work" $ eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "x"))) @?= Right (ValInt 6)

testLet1 :: TestTree
testLet1 = testCase "should work" $ eval envEmpty (Let "x" (CstInt 3) (Add (Var "x") (Var "y"))) @?= Left "Error: Variable not found in environment"