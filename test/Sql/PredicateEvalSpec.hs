{-# LANGUAGE OverloadedStrings #-}

module Sql.PredicateEvalSpec (tests) where

import Data.Text (Text)
import Database.PostgresInterface.Queryable (ColumnValue (..))
import Database.PostgresInterface.Sql.Plan
  ( CompOp (..)
  , Predicate (..)
  , ScalarLiteral (..)
  )
import Database.PostgresInterface.Sql.PredicateEval
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Sql.PredicateEval"
  [ testGroup "evalPredicate — ColOp"
      [ testCase "text Eq match"          testTextEqMatch
      , testCase "text Eq no match"       testTextEqNoMatch
      , testCase "int Gt true"            testIntGtTrue
      , testCase "int Gt false"           testIntGtFalse
      , testCase "int Le true"            testIntLeTrue
      , testCase "ColumnNotFound"         testColumnNotFound
      , testCase "TypeMismatch"           testTypeMismatch
      ]
  , testGroup "evalPredicate — Like / ILike"
      [ testCase "LIKE prefix match"      testLikePrefix
      , testCase "LIKE no match"          testLikeNoMatch
      , testCase "LIKE single-char _"     testLikeUnderscore
      , testCase "ILIKE case insensitive" testILike
      , testCase "LIKE on non-text col"   testLikeTypeMismatch
      ]
  , testGroup "evalPredicate — NULL checks"
      [ testCase "IsNull true"            testIsNullTrue
      , testCase "IsNull false"           testIsNullFalse
      , testCase "IsNull missing col"     testIsNullMissing
      , testCase "IsNotNull true"         testIsNotNullTrue
      , testCase "IsNotNull on null"      testIsNotNullOnNull
      ]
  , testGroup "evalPredicate — bool checks"
      [ testCase "IsTrue true"            testIsTrueTrue
      , testCase "IsTrue false"           testIsTrueFalse
      , testCase "IsFalse true"           testIsFalseTrue
      ]
  , testGroup "evalPredicate — ColIn / ColNotIn"
      [ testCase "ColIn found"            testColInFound
      , testCase "ColIn not found"        testColInNotFound
      , testCase "ColNotIn not in list"   testColNotInNotFound
      , testCase "ColNotIn in list"       testColNotInFound
      ]
  , testGroup "evalPredicate — ColCmpCol"
      [ testCase "equal columns"          testColCmpColEq
      , testCase "unequal columns"        testColCmpColNe
      , testCase "missing second col"     testColCmpColMissing
      ]
  , testGroup "evalPredicate — combinators"
      [ testCase "And both true"          testAndBothTrue
      , testCase "And one false"          testAndOneFalse
      , testCase "Or one true"            testOrOneTrue
      , testCase "Or both false"          testOrBothFalse
      , testCase "Not true → false"       testNotTrue
      , testCase "Not false → true"       testNotFalse
      , testCase "And short-circuits error" testAndError
      ]
  , testGroup "evalPredicates"
      [ testCase "empty list is true"     testEmptyPreds
      , testCase "all pass"               testAllPass
      , testCase "one fails"              testOneFails
      ]
  , testGroup "compareColumnValues"
      [ testCase "equal ints"             testCmpEqInts
      , testCase "ordered ints"           testCmpOrdInts
      , testCase "null < non-null"        testCmpNullLt
      , testCase "null == null"           testCmpNullEq
      , testCase "non-null > null"        testCmpNonNullGt
      , testCase "mismatched types → EQ" testCmpMismatch
      ]
  ]

-- Helpers ---------------------------------------------------------------------

row :: [(Text, ColumnValue)] -> Row
row = id

assertRight :: (Show e, HasCallStack) => Either e Bool -> Bool -> Assertion
assertRight (Left e)  _ = assertFailure ("unexpected Left: " <> show e)
assertRight (Right b) expected = b @?= expected

assertLeft :: (Show b, HasCallStack) => Either EvalError b -> EvalError -> Assertion
assertLeft (Right b) _ = assertFailure ("expected Left, got Right: " <> show b)
assertLeft (Left e) expected = e @?= expected

-- ColOp tests -----------------------------------------------------------------

testTextEqMatch :: Assertion
testTextEqMatch =
  assertRight
    (evalPredicate (row [("name", PgTextVal "alice")]) (ColOp "name" Eq (LitText "alice")))
    True

testTextEqNoMatch :: Assertion
testTextEqNoMatch =
  assertRight
    (evalPredicate (row [("name", PgTextVal "bob")]) (ColOp "name" Eq (LitText "alice")))
    False

testIntGtTrue :: Assertion
testIntGtTrue =
  assertRight
    (evalPredicate (row [("age", PgInt4Val 30)]) (ColOp "age" Gt (LitInt 25)))
    True

testIntGtFalse :: Assertion
testIntGtFalse =
  assertRight
    (evalPredicate (row [("age", PgInt4Val 20)]) (ColOp "age" Gt (LitInt 25)))
    False

testIntLeTrue :: Assertion
testIntLeTrue =
  assertRight
    (evalPredicate (row [("age", PgInt4Val 25)]) (ColOp "age" Le (LitInt 25)))
    True

testColumnNotFound :: Assertion
testColumnNotFound =
  assertLeft
    (evalPredicate (row [("age", PgInt4Val 30)]) (ColOp "missing" Eq (LitInt 30)))
    (ColumnNotFound "missing")

testTypeMismatch :: Assertion
testTypeMismatch =
  assertLeft
    (evalPredicate (row [("name", PgTextVal "alice")]) (ColOp "name" Eq (LitInt 1)))
    (TypeMismatch (PgTextVal "alice") (LitInt 1))

-- Like / ILike tests ----------------------------------------------------------

testLikePrefix :: Assertion
testLikePrefix =
  assertRight
    (evalPredicate (row [("s", PgTextVal "hello world")]) (Like "s" "hello%"))
    True

testLikeNoMatch :: Assertion
testLikeNoMatch =
  assertRight
    (evalPredicate (row [("s", PgTextVal "goodbye")]) (Like "s" "hello%"))
    False

testLikeUnderscore :: Assertion
testLikeUnderscore =
  assertRight
    (evalPredicate (row [("s", PgTextVal "cat")]) (Like "s" "c_t"))
    True

testILike :: Assertion
testILike =
  assertRight
    (evalPredicate (row [("s", PgTextVal "Hello")]) (ILike "s" "hello"))
    True

testLikeTypeMismatch :: Assertion
testLikeTypeMismatch =
  assertLeft
    (evalPredicate (row [("n", PgInt4Val 42)]) (Like "n" "%"))
    (TypeMismatch (PgInt4Val 42) (LitText "%"))

-- NULL check tests ------------------------------------------------------------

testIsNullTrue :: Assertion
testIsNullTrue =
  assertRight
    (evalPredicate (row [("x", PgNull)]) (IsNull "x"))
    True

testIsNullFalse :: Assertion
testIsNullFalse =
  assertRight
    (evalPredicate (row [("x", PgTextVal "y")]) (IsNull "x"))
    False

testIsNullMissing :: Assertion
testIsNullMissing =
  assertLeft
    (evalPredicate (row [("x", PgNull)]) (IsNull "missing"))
    (ColumnNotFound "missing")

testIsNotNullTrue :: Assertion
testIsNotNullTrue =
  assertRight
    (evalPredicate (row [("x", PgInt4Val 1)]) (IsNotNull "x"))
    True

testIsNotNullOnNull :: Assertion
testIsNotNullOnNull =
  assertRight
    (evalPredicate (row [("x", PgNull)]) (IsNotNull "x"))
    False

-- Bool check tests ------------------------------------------------------------

testIsTrueTrue :: Assertion
testIsTrueTrue =
  assertRight
    (evalPredicate (row [("ok", PgBoolVal True)]) (IsTrue "ok"))
    True

testIsTrueFalse :: Assertion
testIsTrueFalse =
  assertRight
    (evalPredicate (row [("ok", PgBoolVal False)]) (IsTrue "ok"))
    False

testIsFalseTrue :: Assertion
testIsFalseTrue =
  assertRight
    (evalPredicate (row [("ok", PgBoolVal False)]) (IsFalse "ok"))
    True

-- ColIn / ColNotIn tests ------------------------------------------------------

testColInFound :: Assertion
testColInFound =
  assertRight
    (evalPredicate (row [("x", PgInt4Val 2)]) (ColIn "x" [LitInt 1, LitInt 2, LitInt 3]))
    True

testColInNotFound :: Assertion
testColInNotFound =
  assertRight
    (evalPredicate (row [("x", PgInt4Val 5)]) (ColIn "x" [LitInt 1, LitInt 2, LitInt 3]))
    False

testColNotInNotFound :: Assertion
testColNotInNotFound =
  assertRight
    (evalPredicate (row [("x", PgInt4Val 5)]) (ColNotIn "x" [LitInt 1, LitInt 2, LitInt 3]))
    True

testColNotInFound :: Assertion
testColNotInFound =
  assertRight
    (evalPredicate (row [("x", PgInt4Val 2)]) (ColNotIn "x" [LitInt 1, LitInt 2, LitInt 3]))
    False

-- ColCmpCol tests -------------------------------------------------------------

testColCmpColEq :: Assertion
testColCmpColEq =
  assertRight
    (evalPredicate (row [("a", PgInt4Val 5), ("b", PgInt4Val 5)]) (ColCmpCol "a" Eq "b"))
    True

testColCmpColNe :: Assertion
testColCmpColNe =
  assertRight
    (evalPredicate (row [("a", PgInt4Val 5), ("b", PgInt4Val 6)]) (ColCmpCol "a" Eq "b"))
    False

testColCmpColMissing :: Assertion
testColCmpColMissing =
  assertLeft
    (evalPredicate (row [("a", PgInt4Val 5)]) (ColCmpCol "a" Eq "b"))
    (ColumnNotFound "b")

-- Combinator tests ------------------------------------------------------------

testAndBothTrue :: Assertion
testAndBothTrue =
  assertRight
    (evalPredicate r (And (ColOp "x" Eq (LitInt 1)) (ColOp "y" Eq (LitInt 2))))
    True
  where r = row [("x", PgInt4Val 1), ("y", PgInt4Val 2)]

testAndOneFalse :: Assertion
testAndOneFalse =
  assertRight
    (evalPredicate r (And (ColOp "x" Eq (LitInt 1)) (ColOp "y" Eq (LitInt 9))))
    False
  where r = row [("x", PgInt4Val 1), ("y", PgInt4Val 2)]

testOrOneTrue :: Assertion
testOrOneTrue =
  assertRight
    (evalPredicate r (Or (ColOp "x" Eq (LitInt 99)) (ColOp "y" Eq (LitInt 2))))
    True
  where r = row [("x", PgInt4Val 1), ("y", PgInt4Val 2)]

testOrBothFalse :: Assertion
testOrBothFalse =
  assertRight
    (evalPredicate r (Or (ColOp "x" Eq (LitInt 99)) (ColOp "y" Eq (LitInt 99))))
    False
  where r = row [("x", PgInt4Val 1), ("y", PgInt4Val 2)]

testNotTrue :: Assertion
testNotTrue =
  assertRight
    (evalPredicate (row [("x", PgInt4Val 1)]) (Not (ColOp "x" Eq (LitInt 1))))
    False

testNotFalse :: Assertion
testNotFalse =
  assertRight
    (evalPredicate (row [("x", PgInt4Val 1)]) (Not (ColOp "x" Eq (LitInt 99))))
    True

testAndError :: Assertion
testAndError =
  assertLeft
    (evalPredicate (row [("x", PgInt4Val 1)]) (And (ColOp "missing" Eq (LitInt 1)) (ColOp "x" Eq (LitInt 1))))
    (ColumnNotFound "missing")

-- evalPredicates tests --------------------------------------------------------

testEmptyPreds :: Assertion
testEmptyPreds =
  assertRight (evalPredicates (row [("x", PgInt4Val 1)]) []) True

testAllPass :: Assertion
testAllPass =
  assertRight
    (evalPredicates r [ColOp "x" Eq (LitInt 1), ColOp "y" Gt (LitInt 0)])
    True
  where r = row [("x", PgInt4Val 1), ("y", PgInt4Val 5)]

testOneFails :: Assertion
testOneFails =
  assertRight
    (evalPredicates r [ColOp "x" Eq (LitInt 1), ColOp "y" Gt (LitInt 10)])
    False
  where r = row [("x", PgInt4Val 1), ("y", PgInt4Val 5)]

-- compareColumnValues tests ---------------------------------------------------

testCmpEqInts :: Assertion
testCmpEqInts =
  compareColumnValues (Just (PgInt4Val 3)) (Just (PgInt4Val 3)) @?= EQ

testCmpOrdInts :: Assertion
testCmpOrdInts =
  compareColumnValues (Just (PgInt4Val 1)) (Just (PgInt4Val 2)) @?= LT

testCmpNullLt :: Assertion
testCmpNullLt =
  compareColumnValues Nothing (Just (PgInt4Val 1)) @?= LT

testCmpNullEq :: Assertion
testCmpNullEq =
  compareColumnValues Nothing Nothing @?= EQ

testCmpNonNullGt :: Assertion
testCmpNonNullGt =
  compareColumnValues (Just (PgInt4Val 1)) Nothing @?= GT

testCmpMismatch :: Assertion
testCmpMismatch =
  compareColumnValues (Just (PgInt4Val 1)) (Just (PgTextVal "a")) @?= EQ
