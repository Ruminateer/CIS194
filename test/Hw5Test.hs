module Hw5Test where

import Hw5.Calc
import Hw5.ExprT
import Hw5.Parser
import Test.Tasty
import Test.Tasty.HUnit

testAll :: TestTree
testAll =
  testGroup
    "Hw5"
    [ testEval,
      testExprInstances
    ]

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

testExp :: (Expr a) => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testExprInstances :: TestTree
testExprInstances =
  testGroup
    "Expr instances"
    [ testCase
        "Integer"
        ((Just (-7)) @=? (testExp :: Maybe Integer)),
      testCase
        "Bool"
        ((Just True) @=? (testExp :: Maybe Bool)),
      testCase
        "MinMax"
        ((Just (MinMax 5)) @=? (testExp :: Maybe MinMax)),
      testCase
        "Mod7"
        ((Just (Mod7 0)) @=? (testExp :: Maybe Mod7))
    ]
