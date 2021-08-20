import Control.Monad (filterM, liftM, liftM2, liftM3)
import qualified Core.ArithC as A
import qualified Core.Deferred as D
import qualified Core.ExprC as E
import Data.List (find)
import Data.Maybe (isNothing)
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen, oneof, sized)
import Test.QuickCheck.Property (Discard (Discard), Testable, forAll, property)

testNames :: [E.Name]
testNames = ["a", "b", "c", "d", "e"]

testName :: Gen E.Name
testName = oneof $ map return testNames

testIdentifiers :: [E.Identifier]
testIdentifiers = ["l", "m", "n", "o", "p"]

testIdentifier :: Gen E.Identifier
testIdentifier = oneof $ map return testIdentifiers

testExpr :: Gen E.ExprC
testExpr = sized genExpr
  where
    genExpr size
      | size <= 0 = oneof [fmap E.Value arbitrary, fmap E.IdC testIdentifier]
      | otherwise =
        oneof
          [ liftM2 E.Add subExpr subExpr,
            liftM2 E.Mul subExpr subExpr,
            liftM2 E.AppC testName inputExpr
          ]
      where
        subExpr = genExpr (size `div` 2)
        inputExpr = genExpr (size - 1)

powerset :: [a] -> [[a]]
powerset = filterM (const [True, False])

testFunDefs :: Gen [E.FunDefC]
-- TODO: Understand what mapM is doing here...
testFunDefs = oneof $ map (mapM genFunDef) (powerset testNames)
  where
    genFunDef :: E.Name -> Gen E.FunDefC
    genFunDef name = fmap (E.FunDefC name "x") testExpr

testInput :: Gen ([E.FunDefC], E.ExprC)
testInput = (,) <$> testFunDefs <*> testExpr

main :: IO ()
main = hspec $ do
  describe "interp with ArithC" $ do
    it "returns one value" $ do
      A.interp (A.Value 5) `shouldBe` 5
    it "adds two values" $ do
      A.interp (A.Add (A.Value 7) (A.Value 10)) `shouldBe` 17
    it "multiplies two values" $ do
      A.interp (A.Mul (A.Value 2) (A.Value 25)) `shouldBe` 50
    it "adds and multiplies three values" $ do
      A.interp (A.Add (A.Value 37) (A.Mul (A.Value 15) (A.Value (-3)))) `shouldBe` -8
    it "adds and multiplies many values" $ do
      A.interp
        ( A.Mul
            (A.Add (A.Value 5) (A.Mul (A.Value (-22)) (A.Value (-2))))
            (A.Mul (A.Add (A.Value 15) (A.Value 100)) (A.Value 10))
        )
        `shouldBe` 56350
  describe "interp with ExprC" $ do
    testInterpExprC E.interp
  describe "interp with ExprC (deferred substitution)" $ do
    testInterpExprC D.interp
    it "behaves the same as non-deferred substitution" $
      forAll testInput (\(funDefs, expr) -> E.interp funDefs expr `shouldBe` D.interp funDefs expr)

testInterpExprC :: ([E.FunDefC] -> E.ExprC -> E.InterpResult Int) -> SpecWith ()
testInterpExprC interpExprC = do
  it "applies a simple function" $ do
    interpExprC
      [E.FunDefC "square" "xx" (E.Mul (E.IdC "xx") (E.IdC "xx"))]
      (E.AppC "square" (E.Value 5))
      `shouldBe` Right 25
  it "applies the same function twice" $ do
    interpExprC
      [E.FunDefC "square" "xx" (E.Mul (E.IdC "xx") (E.IdC "xx"))]
      (E.Add (E.AppC "square" (E.Value 3)) (E.AppC "square" (E.Value 4)))
      `shouldBe` Right 25
  it "applies a function within an expression" $ do
    interpExprC
      [E.FunDefC "adder" "yy" (E.Add (E.IdC "yy") (E.Value 8))]
      (E.Mul (E.Value 5) (E.AppC "adder" (E.Mul (E.Value (-10)) (E.Value 3))))
      `shouldBe` Right (-110)
  it "applies a nested function" $ do
    interpExprC
      [ E.FunDefC "outer" "oo" (E.Add (E.Value 10) (E.AppC "inner" (E.IdC "oo"))),
        E.FunDefC "inner" "ii" (E.Mul (E.Value 3) (E.IdC "ii"))
      ]
      (E.AppC "outer" (E.Value 5))
      `shouldBe` Right 25
  it "returns an error for an undefined function" $ do
    interpExprC [] (E.AppC "square" (E.Value 5)) `shouldBe` Left (E.UndefinedFunction "square")
  it "returns an error for an unbound identifier" $ do
    interpExprC [] (E.Add (E.Value 10) (E.IdC "zz")) `shouldBe` Left (E.UnboundIdentifier "zz")
  it "returns an error for an invalid expression provided as a function argument" $ do
    interpExprC
      [E.FunDefC "square" "xx" (E.Mul (E.IdC "xx") (E.IdC "xx"))]
      (E.AppC "square" (E.IdC "yy"))
      `shouldBe` Left (E.UnboundIdentifier "yy")
  it "reports a valid undefined function" $
    -- In case of undefined function, reported function name appears in testNames
    forAll
      testInput
      ( \(funDefs, expr) -> case interpExprC funDefs expr of
          Left (E.UndefinedFunction name) ->
            property
              (name `elem` testNames)
          _ -> property Discard
      )
  it "reports an undefined function that was not provided" $
    -- In case of undefined function, reported function name does not appear in
    -- given list of function names
    forAll
      testInput
      ( \(funDefs, expr) -> case interpExprC funDefs expr of
          Left (E.UndefinedFunction missingName) ->
            property
              (isNothing (find (\(E.FunDefC name _ _) -> name == missingName) funDefs))
          _ -> property Discard
      )

-- Crashes with out of memory error... Maybe unbound identifiers are too rare?
-- it "reports a valid unbound identifier" $
--   -- In case of unbound identifier, reported identifer appears in testIdentifiers
--   forAll
--     testInput
--     ( \(funDefs, expr) -> case interpExprC funDefs expr of
--         Left (E.UnboundIdentifier identifier) ->
--           property
--             (identifier `elem` testIdentifiers)
--         _ -> property Discard
--     )