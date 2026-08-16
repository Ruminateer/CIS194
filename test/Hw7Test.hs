module Hw7Test (testAll) where

import Hw7.JoinList (JoinList (..), dropJ, indexJ, takeJ, (+++))
import Hw7.Sized (Size (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

testAll :: TestTree
testAll =
  testGroup
    "Hw7"
    [ exercise1Tests,
      exercise2Tests
    ]

exercise1Tests :: TestTree
exercise1Tests =
  testGroup
    "Exercise 1: (+++)"
    [ testCase "combines annotations in left-to-right order" $ do
        let left =
              Append
                "ab"
                (Single "a" (1 :: Int))
                (Single "b" 2)
            right = Single "c" 3
            result = left +++ right
        rootAnnotation result @?= "abc"
        joinListToList result @?= [1, 2, 3],
      testCase "uses mempty as the annotation of Empty" $ do
        let empty = Empty :: JoinList String Int
            item = Single "item" 1
            emptyResult = empty +++ empty
            leftResult = empty +++ item
            rightResult = item +++ empty
        rootAnnotation emptyResult @?= ""
        rootAnnotation leftResult @?= "item"
        rootAnnotation rightResult @?= "item"
        joinListToList leftResult @?= [1]
        joinListToList rightResult @?= [1],
      testCase "preserves valid annotations throughout its result" $ do
        let left =
              Append
                "left"
                (Single "le" 'x')
                (Single "ft" 'y')
            right =
              Append
                "right"
                (Single "ri" 'z')
                (Single "ght" '!')
            result = left +++ right
        assertBool "every Append annotation should combine its child annotations" $
          annotationsAreValid result
    ]

exercise2Tests :: TestTree
exercise2Tests =
  testGroup
    "Exercise 2: indexJ, dropJ, and takeJ"
    [ testGroup "indexJ agrees with safe list indexing" $
        map indexTestsFor fixtures,
      testGroup "dropJ agrees with list drop" $
        map dropTestsFor fixtures,
      testGroup "takeJ agrees with list take" $
        map takeTestsFor fixtures
    ]

indexTestsFor :: (String, JoinList Size Char) -> TestTree
indexTestsFor (fixtureName, tree) =
  testCase fixtureName $
    mapM_
      ( \index ->
          assertEqual
            ("index " ++ show index)
            (safeIndex index values)
            (indexJ index tree)
      )
      testIndices
  where
    values = joinListToList tree
    testIndices = [-2 .. length values + 2]

dropTestsFor :: (String, JoinList Size Char) -> TestTree
dropTestsFor (fixtureName, tree) =
  testCase fixtureName $
    mapM_
      ( \count -> do
          let result = dropJ count tree
          assertEqual
            ("drop " ++ show count)
            (drop count values)
            (joinListToList result)
          assertBool
            ("drop " ++ show count ++ " should preserve cached sizes")
            (sizeAnnotationsAreValid result)
      )
      testCounts
  where
    values = joinListToList tree
    testCounts = [-2 .. length values + 2]

takeTestsFor :: (String, JoinList Size Char) -> TestTree
takeTestsFor (fixtureName, tree) =
  testCase fixtureName $
    mapM_
      ( \count -> do
          let result = takeJ count tree
          assertEqual
            ("take " ++ show count)
            (take count values)
            (joinListToList result)
          assertBool
            ("take " ++ show count ++ " should preserve cached sizes")
            (sizeAnnotationsAreValid result)
      )
      testCounts
  where
    values = joinListToList tree
    testCounts = [-2 .. length values + 2]

fixtures :: [(String, JoinList Size Char)]
fixtures =
  [ ("empty", Empty),
    ("singleton", sizedLeaf 'a'),
    ("balanced tree", balancedTree),
    ("right-heavy tree", rightHeavyTree),
    ("tree with Empty branches", treeWithEmptyBranches)
  ]

balancedTree :: JoinList Size Char
balancedTree =
  Append
    4
    (Append 2 (sizedLeaf 'a') (sizedLeaf 'b'))
    (Append 2 (sizedLeaf 'c') (sizedLeaf 'd'))

rightHeavyTree :: JoinList Size Char
rightHeavyTree =
  Append
    5
    (sizedLeaf 'a')
    ( Append
        4
        ( Append
            3
            (sizedLeaf 'b')
            (Append 2 (sizedLeaf 'c') (sizedLeaf 'd'))
        )
        (sizedLeaf 'e')
    )

treeWithEmptyBranches :: JoinList Size Char
treeWithEmptyBranches =
  Append
    4
    (Append 2 Empty (Append 2 (sizedLeaf 'a') (sizedLeaf 'b')))
    (Append 2 (Append 2 (sizedLeaf 'c') (sizedLeaf 'd')) Empty)

sizedLeaf :: a -> JoinList Size a
sizedLeaf = Single 1

safeIndex :: Int -> [a] -> Maybe a
safeIndex index _ | index < 0 = Nothing
safeIndex _ [] = Nothing
safeIndex 0 (value : _) = Just value
safeIndex index (_ : rest) = safeIndex (index - 1) rest

joinListToList :: JoinList m a -> [a]
joinListToList Empty = []
joinListToList (Single _ value) = [value]
joinListToList (Append _ left right) =
  joinListToList left ++ joinListToList right

rootAnnotation :: (Monoid m) => JoinList m a -> m
rootAnnotation Empty = mempty
rootAnnotation (Single annotation _) = annotation
rootAnnotation (Append annotation _ _) = annotation

annotationsAreValid :: (Eq m, Monoid m) => JoinList m a -> Bool
annotationsAreValid Empty = True
annotationsAreValid (Single _ _) = True
annotationsAreValid (Append annotation left right) =
  annotation == rootAnnotation left <> rootAnnotation right
    && annotationsAreValid left
    && annotationsAreValid right

sizeAnnotationsAreValid :: JoinList Size a -> Bool
sizeAnnotationsAreValid Empty = True
sizeAnnotationsAreValid (Single annotation _) = annotation == 1
sizeAnnotationsAreValid (Append annotation left right) =
  annotation == rootAnnotation left <> rootAnnotation right
    && sizeAnnotationsAreValid left
    && sizeAnnotationsAreValid right
