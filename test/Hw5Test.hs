module Hw5Test where

import Hw5.Calc
import Hw5.ExprT
import Test.Tasty
import Test.Tasty.HUnit

testEval :: TestTree
testEval =
  testGroup
    "eval"
    [ testCase
        "(Lit 3)"
        ( 3
            @=? eval (Lit 3)
        ),
      testCase
        "(Mul (Add (Lit 2) (Lit 3)) (Lit 4))"
        ( 20
            @=? eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
        ),
      testCase
        "(Add (Lit 2) (Mul (Lit 3) (Lit 4)))"
        ( 14
            @=? eval (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
        ),
      testCase
        "(Mul (Mul (Lit 2) (Add (Lit 3) (Lit 4))) (Add (Lit 4) (Lit 5)))"
        ( 126
            @=? eval (Mul (Mul (Lit 2) (Add (Lit 3) (Lit 4))) (Add (Lit 4) (Lit 5)))
        )
    ]

testAll :: TestTree
testAll =
  testGroup
    "Hw5"
    [ testEval
    ]
