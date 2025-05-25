import Test.Hspec
import qualified Gode.Core as Gode

main :: IO ()
main = hspec $ do
  describe "GODE" $ do
    describe "Greater than operator" $ do
      it "evaluates x > 3 correctly for value above threshold" $ do
        let env = Gode.Env 4 "test"
            (result, logs) = Gode.eval Gode.program env
        result `shouldBe` "high"
        length logs `shouldBe` 3
        head logs `shouldContain` "is True"

      it "evaluates x > 3 correctly for value below threshold" $ do
        let env = Gode.Env 2 "test"
            (result, logs) = Gode.eval Gode.program env
        result `shouldBe` "low"
        length logs `shouldBe` 3
        head logs `shouldContain` "is False"

    describe "Less than operator" $ do
      it "evaluates x < 3 correctly for value below threshold" $ do
        let env = Gode.Env 2 "test"
            program = Gode.If "x < 3" "low" "high"
            (result, logs) = Gode.eval program env
        result `shouldBe` "low"
        length logs `shouldBe` 3
        head logs `shouldContain` "is True"

      it "evaluates x < 3 correctly for value above threshold" $ do
        let env = Gode.Env 4 "test"
            program = Gode.If "x < 3" "low" "high"
            (result, logs) = Gode.eval program env
        result `shouldBe` "high"
        length logs `shouldBe` 3
        head logs `shouldContain` "is False"

    describe "Equals operator" $ do
      it "evaluates x == 3 correctly for matching value" $ do
        let env = Gode.Env 3 "test"
            program = Gode.If "x == 3" "match" "no-match"
            (result, logs) = Gode.eval program env
        result `shouldBe` "match"
        length logs `shouldBe` 3
        head logs `shouldContain` "is True"

      it "evaluates x == 3 correctly for non-matching value" $ do
        let env = Gode.Env 4 "test"
            program = Gode.If "x == 3" "match" "no-match"
            (result, logs) = Gode.eval program env
        result `shouldBe` "no-match"
        length logs `shouldBe` 3
        head logs `shouldContain` "is False"