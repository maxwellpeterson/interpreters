import Control.Monad (liftM2)
import qualified Core.ArithC as A
import qualified Core.Deferred as D
import qualified Core.ExprC as E
import qualified Core.LamExprC as L
import qualified Core.BoxExprC as B
import Test.Hspec (SpecWith, describe, hspec, it, shouldBe, shouldNotSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen (Gen, elements, frequency, oneof, sized)
import Test.QuickCheck.Property (Discard (Discard), Testable, label, property)

-- TODO: Figure out how to cleanly create different custom generator
-- expressions. Each of the `prop` test cases below would benefit from a
-- custom-tailored generator, with different distributions of undefined
-- functions, unbound identifiers, etc. Same situation for generating function
-- bodies that can't create infinite loops.

testNames :: [E.Name]
testNames = ["linear", "cubic", "foo"]

testNameFreqs :: [Int]
testNameFreqs = [40, 30, 1]

testFunDefs :: [E.FunDefC Int]
testFunDefs =
  [ -- linear(a) = 5a + 15
    E.FunDefC "linear" "aa" (E.Add (E.Mul (E.Value 5) (E.IdC "aa")) (E.Value 15)),
    -- cubic(b) = b^3 + 5b^2 + 15b
    E.FunDefC
      "cubic"
      "bb"
      ( E.Add
          (E.Mul (E.IdC "bb") (E.Mul (E.IdC "bb") (E.IdC "bb")))
          (E.Mul (E.IdC "bb") (E.AppC "linear" (E.IdC "bb")))
      )
  ]

instance Arbitrary a => Arbitrary (E.ExprC a) where
  arbitrary = sized genExpr
    where
      genExpr size
        -- Don't insert arbitrary identifiers into expressions
        | size <= 0 = oneof [fmap E.Value arbitrary]
        | otherwise =
          oneof
            [ liftM2 E.Add left right,
              liftM2 E.Mul left right,
              liftM2
                E.AppC
                (frequency (zipWith (\name weight -> (weight, return name)) testNames testNameFreqs))
                inputExpr
            ]
        where
          left = genExpr (size `div` 2)
          right = genExpr (size `div` 2)
          inputExpr = genExpr (size - 1)

-- DANGER!! Re-using the default expression generator for function bodies may
-- create programs that don't terminate, since allowing arbitrary function calls
-- in function bodies can result in infinite loops.

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
  describe "compare substitution strategies" $ do
    prop "both strategies give the same result" $
      \expr ->
        let firstResult = E.interp testFunDefs expr
         in label (labelResult firstResult) $ firstResult `shouldBe` D.interp testFunDefs expr
  describe "interp with LamExprC" $ do
    it "returns a closure" $
      do
        L.interp (L.AppC (L.LamC "x" (L.LamC "y" (L.Add (L.IdC "x") (L.IdC "y")))) (L.Value 10))
        `shouldBe` L.ClosureResult (L.Closure "y" (L.Add (L.IdC "x") (L.IdC "y")) [("x", L.Value 10)])
    it "handles a higher order function" $
      do
        L.interp (L.AppC (L.LamC "f" (L.AppC (L.IdC "f") (L.Value 5))) (L.LamC "x" (L.Mul (L.IdC "x") (L.IdC "x"))))
        `shouldBe` L.ValueResult 25
    it "handles nested closure application" $
      do
        L.interp (L.AppC (L.AppC (L.LamC "x" (L.LamC "y" (L.Add (L.IdC "x") (L.IdC "y")))) (L.Value 10)) (L.Value 5))
        `shouldBe` L.ValueResult 15
    it "returns an error for unexpected value" $ do
      L.interp (L.AppC (L.Value 5) (L.Value 10)) `shouldBe` L.ErrorResult L.UnexpectedValue
    it "returns an error for unexpected closure" $ do
      L.interp (L.Add (L.Value 5) (L.LamC "x" (L.IdC "x"))) `shouldBe` L.ErrorResult L.UnexpectedClosure
  describe "interp with BoxExprC" $ do
    testBoxExprC

labelResult :: E.InterpResult a -> String
labelResult (Left (E.UndefinedFunction _)) = "UndefinedFunction"
labelResult (Left (E.UnboundIdentifier _)) = "UnboundIdentifier"
labelResult (Right _) = "Value"

testInterpExprC :: ([E.FunDefC Int] -> E.ExprC Int -> E.InterpResult Int) -> SpecWith ()
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
  it "applies a function from within another function" $ do
    interpExprC testFunDefs (E.AppC "cubic" (E.Value 10)) `shouldBe` Right 1650
  it "returns an error for an undefined function" $ do
    interpExprC [] (E.AppC "square" (E.Value 5)) `shouldBe` Left (E.UndefinedFunction "square")
  it "returns an error for an unbound identifier" $ do
    interpExprC [] (E.Add (E.Value 10) (E.IdC "zz")) `shouldBe` Left (E.UnboundIdentifier "zz")
  it "returns an error for an invalid expression provided as a function argument" $ do
    interpExprC
      [E.FunDefC "square" "xx" (E.Mul (E.IdC "xx") (E.IdC "xx"))]
      (E.AppC "square" (E.IdC "yy"))
      `shouldBe` Left (E.UnboundIdentifier "yy")
  prop "undefined function name appears in testNames" $
    \expr -> case interpExprC testFunDefs expr of
      Left (E.UndefinedFunction name) ->
        property
          (name `elem` testNames)
      _ -> property Discard
  prop "undefined function name does NOT appear in testFunDefs" $
    \expr -> case interpExprC testFunDefs expr of
      Left (E.UndefinedFunction name) ->
        property
          (name `notElem` map (\(E.FunDefC name _ _) -> name) testFunDefs)
      _ -> property Discard
  -- As defined, our generator will never create unbound identifiers
  prop "does NOT report unbound identifier" $
    \expr ->
      let unboundIdentifier :: E.InterpResult a -> Bool
          unboundIdentifier (Left (E.UnboundIdentifier _)) = True
          unboundIdentifier _ = False
       in interpExprC testFunDefs expr `shouldNotSatisfy` unboundIdentifier

testBoxExprC :: SpecWith ()
testBoxExprC = do
  it "boxes and unboxes values" $
    B.interp (B.Unbox (B.Box (B.Value 5))) `shouldBe` Right (B.ValueResult 5)
  it "allows boxed values to be mutated" $
    B.interp (B.AppC (B.LamC "box" (B.Seq (B.SetBox (B.IdC "box") (B.Value 10)) (B.Unbox (B.IdC "box")))) (B.Box (B.Value 5))) `shouldBe` Right (B.ValueResult 10)
