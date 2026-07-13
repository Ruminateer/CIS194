module Hw5Test (testAll) where

import Hw5.Calc
import qualified Hw5.ExprT as E
import qualified Hw5.Parser as Parser
import qualified Hw5.StackVM as VM
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

testAll :: TestTree
testAll =
  testGroup
    "Hw5"
    [ exercise1Tests,
      exercise2Tests,
      exercise3Tests,
      exercise4Tests,
      exercise5Tests
    ]

exercise1Tests :: TestTree
exercise1Tests =
  testGroup
    "Exercise 1: eval"
    [ testCase "evaluates a literal" $
        eval (E.Lit 3) @?= 3,
      testCase "preserves a negative literal" $
        eval (E.Lit (-8)) @?= -8,
      testCase "evaluates addition" $
        eval (E.Add (E.Lit 7) (E.Lit (-2))) @?= 5,
      testCase "evaluates multiplication" $
        eval (E.Mul (E.Lit (-3)) (E.Lit 6)) @?= -18,
      testCase "evaluates a nested expression" $
        eval (E.Mul (E.Add (E.Lit 2) (E.Lit 3)) (E.Lit 4)) @?= 20,
      testCase "respects the supplied expression tree" $
        eval (E.Add (E.Lit 2) (E.Mul (E.Lit 3) (E.Lit 4))) @?= 14
    ]

exercise2Tests :: TestTree
exercise2Tests =
  testGroup
    "Exercise 2: evalStr"
    [ testCase "evaluates a literal string" $
        evalStr "42" @?= Just 42,
      testCase "evaluates the parenthesized example" $
        evalStr "(2+3)*4" @?= Just 20,
      testCase "uses multiplication precedence in the documented example" $
        evalStr "2+3*4" @?= Just 14,
      testCase "accepts negative integer literals and spaces" $
        evalStr "(3 * -4) + 5" @?= Just (-7),
      testCase "parentheses can change the result" $
        evalStr "2*(3+4)" @?= Just 14,
      testCase "rejects a missing operand" $
        evalStr "2+3*" @?= Nothing,
      testCase "rejects an empty expression" $
        evalStr "" @?= Nothing,
      testCase "rejects unbalanced parentheses" $
        evalStr "(2+3" @?= Nothing,
      testCase "rejects unsupported operators" $
        evalStr "8-3" @?= Nothing,
      testCase "rejects trailing input" $
        evalStr "2+3xyz" @?= Nothing
    ]

exercise3Tests :: TestTree
exercise3Tests =
  testGroup
    "Exercise 3: Expr ExprT instance"
    [ testCase "lit constructs Lit" $
        (lit (-5) :: E.ExprT) @?= E.Lit (-5),
      testCase "add constructs Add" $
        (add (lit 2) (lit 3) :: E.ExprT)
          @?= E.Add (E.Lit 2) (E.Lit 3),
      testCase "mul constructs Mul" $
        (mul (lit 2) (lit 3) :: E.ExprT)
          @?= E.Mul (E.Lit 2) (E.Lit 3),
      testCase "nested class operations preserve the expression structure" $
        (mul (add (lit 2) (lit 3)) (lit 4) :: E.ExprT)
          @?= E.Mul (E.Add (E.Lit 2) (E.Lit 3)) (E.Lit 4)
    ]

exercise4Tests :: TestTree
exercise4Tests =
  testGroup
    "Exercise 4: alternate Expr instances"
    [ integerTests,
      boolTests,
      minMaxTests,
      mod7Tests,
      testCase "one parsed expression can use every interpretation" $ do
        parsedExpression @?= (Just (-7) :: Maybe Integer)
        parsedExpression @?= (Just True :: Maybe Bool)
        parsedExpression @?= Just (MinMax 5)
        parsedExpression @?= Just (Mod7 0)
    ]

integerTests :: TestTree
integerTests =
  testGroup
    "Integer"
    [ testCase "lit is the represented integer" $
        (lit (-9) :: Integer) @?= -9,
      testCase "add is integer addition" $
        (add (lit 8) (lit (-3)) :: Integer) @?= 5,
      testCase "mul is integer multiplication" $
        (mul (lit (-4)) (lit 6) :: Integer) @?= -24
    ]

boolTests :: TestTree
boolTests =
  testGroup
    "Bool"
    [ testCase "negative literals are False" $
        (lit (-1) :: Bool) @?= False,
      testCase "zero is False" $
        (lit 0 :: Bool) @?= False,
      testCase "positive literals are True" $
        (lit 1 :: Bool) @?= True,
      testCase "add is logical or" $ do
        (add (lit 0) (lit 0) :: Bool) @?= False
        (add (lit 0) (lit 2) :: Bool) @?= True,
      testCase "mul is logical and" $ do
        (mul (lit 1) (lit 2) :: Bool) @?= True
        (mul (lit 1) (lit 0) :: Bool) @?= False
    ]

minMaxTests :: TestTree
minMaxTests =
  testGroup
    "MinMax"
    [ testCase "lit preserves the integer" $
        (lit (-8) :: MinMax) @?= MinMax (-8),
      testCase "add chooses the maximum" $
        (add (lit (-3)) (lit 5) :: MinMax) @?= MinMax 5,
      testCase "mul chooses the minimum" $
        (mul (lit (-3)) (lit 5) :: MinMax) @?= MinMax (-3),
      testCase "nested operations use max and min at every node" $
        (mul (add (lit 3) (lit 10)) (lit 7) :: MinMax) @?= MinMax 7
    ]

mod7Tests :: TestTree
mod7Tests =
  testGroup
    "Mod7"
    [ testCase "lit reduces a large positive integer" $
        (lit 15 :: Mod7) @?= Mod7 1,
      testCase "lit normalizes a negative integer into 0 through 6" $
        (lit (-1) :: Mod7) @?= Mod7 6,
      testCase "add is performed modulo 7" $
        (add (lit 5) (lit 3) :: Mod7) @?= Mod7 1,
      testCase "mul is performed modulo 7" $
        (mul (lit 5) (lit 3) :: Mod7) @?= Mod7 1,
      testCase "nested operations remain reduced" $
        (add (mul (lit 6) (lit 6)) (lit 6) :: Mod7) @?= Mod7 0
    ]

parsedExpression :: (Expr a) => Maybe a
parsedExpression = Parser.parseExp lit add mul "(3 * -4) + 5"

exercise5Tests :: TestTree
exercise5Tests =
  testGroup
    "Exercise 5: stack compiler"
    [ testCase "lit emits PushI" $
        assertProgram [VM.PushI (-3)] (lit (-3) :: VM.Program),
      testCase "add emits its operands followed by Add" $
        assertProgram
          [VM.PushI 2, VM.PushI 3, VM.Add]
          (add (lit 2) (lit 3) :: VM.Program),
      testCase "mul emits its operands followed by Mul" $
        assertProgram
          [VM.PushI 2, VM.PushI 3, VM.Mul]
          (mul (lit 2) (lit 3) :: VM.Program),
      testCase "a compiled nested expression has the expected program" $
        assertCompiledProgram
          "(2+3)*4"
          [VM.PushI 2, VM.PushI 3, VM.Add, VM.PushI 4, VM.Mul],
      testCase "compiled precedence example executes to its value" $
        assertCompiledResult "2+3*4" 14,
      testCase "compiled negative literals execute correctly" $
        assertCompiledResult "(3 * -4) + 5" (-7),
      testCase "compile rejects malformed input" $
        case compile "2+3*" of
          Nothing -> pure ()
          Just program -> assertFailure ("expected Nothing, but got " ++ show program)
    ]

assertProgram :: VM.Program -> VM.Program -> Assertion
assertProgram expected actual
  | sameProgram expected actual = pure ()
  | otherwise =
      assertFailure $
        "expected program " ++ show expected ++ ", but got " ++ show actual
  where
    sameProgram xs ys =
      length xs == length ys && and (zipWith sameInstruction xs ys)

    sameInstruction (VM.PushI x) (VM.PushI y) = x == y
    sameInstruction (VM.PushB x) (VM.PushB y) = x == y
    sameInstruction VM.Add VM.Add = True
    sameInstruction VM.Mul VM.Mul = True
    sameInstruction VM.And VM.And = True
    sameInstruction VM.Or VM.Or = True
    sameInstruction _ _ = False

assertCompiledProgram :: String -> VM.Program -> Assertion
assertCompiledProgram input expected =
  case compile input of
    Nothing -> assertFailure ("compile unexpectedly rejected " ++ show input)
    Just actual -> assertProgram expected actual

assertCompiledResult :: String -> Integer -> Assertion
assertCompiledResult input expected =
  case compile input of
    Nothing -> assertFailure ("compile unexpectedly rejected " ++ show input)
    Just program ->
      case VM.stackVM program of
        Right (VM.IVal actual) -> actual @?= expected
        result -> assertFailure ("compiled program produced " ++ show result)
