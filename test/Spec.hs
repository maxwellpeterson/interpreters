import qualified Core.ArithC as A
import qualified Core.ExprC as E
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "interpArithC" $ do
    it "returns one value" $ do
      A.interpArithC (A.Value 5) `shouldBe` 5
    it "adds two values" $ do
      A.interpArithC (A.Add (A.Value 7) (A.Value 10)) `shouldBe` 17
    it "multiplies two values" $ do
      A.interpArithC (A.Mul (A.Value 2) (A.Value 25)) `shouldBe` 50
    it "adds and multiplies three values" $ do
      A.interpArithC (A.Add (A.Value 37) (A.Mul (A.Value 15) (A.Value (-3)))) `shouldBe` -8
    it "adds and multiplies many values" $ do
      A.interpArithC
        ( A.Mul
            (A.Add (A.Value 5) (A.Mul (A.Value (-22)) (A.Value (-2))))
            (A.Mul (A.Add (A.Value 15) (A.Value 100)) (A.Value 10))
        )
        `shouldBe` 56350
  describe "interp with ExprC" $ do
    it "applies a simple function" $ do
      E.interp
        (E.AppC "square" (E.Value 5))
        [E.FunDefC "square" "xx" (E.Mul (E.IdC "xx") (E.IdC "xx"))]
        `shouldBe` Right 25
    it "applies a function within an expression" $ do
      E.interp
        (E.Mul (E.Value 5) (E.AppC "adder" (E.Mul (E.Value (-10)) (E.Value 3))))
        [E.FunDefC "adder" "yy" (E.Add (E.IdC "yy") (E.Value 8))]
        `shouldBe` Right (-110)
    it "applies a nested function" $ do
      E.interp
        (E.AppC "outer" (E.Value 5))
        [ E.FunDefC "outer" "oo" (E.Add (E.Value 10) (E.AppC "inner" (E.IdC "oo"))),
          E.FunDefC "inner" "ii" (E.Mul (E.Value 3) (E.IdC "ii"))
        ]
        `shouldBe` Right 25
    it "returns an error for an undefined function" $ do
      E.interp (E.AppC "square" (E.Value 5)) [] `shouldBe` Left (E.UndefinedFunction "square")
    it "returns an error for an unbound identifier" $ do
      E.interp (E.Add (E.Value 10) (E.IdC "zz")) [] `shouldBe` Left (E.UnboundIdentifier "zz")
