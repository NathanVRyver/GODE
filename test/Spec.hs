import qualified Gode.Core as Gode
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "GODE" $ do
    describe "Greater than operator" $ do
      it "evaluates x > 3 correctly for value above threshold" $ do
        let env = Gode.Env 4 "test"
        case Gode.eval Gode.program env of
          Right (result, logs) -> do
            result `shouldBe` "high"
            length logs `shouldBe` 3
            head logs `shouldContain` "is True"
          Left err -> error $ "Unexpected error: " ++ show err

      it "evaluates x > 3 correctly for value below threshold" $ do
        let env = Gode.Env 2 "test"
        case Gode.eval Gode.program env of
          Right (result, logs) -> do
            result `shouldBe` "low"
            length logs `shouldBe` 3
            head logs `shouldContain` "is False"
          Left err -> error $ "Unexpected error: " ++ show err

    describe "Less than operator" $ do
      it "evaluates x < 3 correctly for value below threshold" $ do
        let env = Gode.Env 2 "test"
            program = Gode.If "x < 3" "low" "high"
        case Gode.eval program env of
          Right (result, logs) -> do
            result `shouldBe` "low"
            length logs `shouldBe` 3
            head logs `shouldContain` "is True"
          Left err -> error $ "Unexpected error: " ++ show err

      it "evaluates x < 3 correctly for value above threshold" $ do
        let env = Gode.Env 4 "test"
            program = Gode.If "x < 3" "low" "high"
        case Gode.eval program env of
          Right (result, logs) -> do
            result `shouldBe` "high"
            length logs `shouldBe` 3
            head logs `shouldContain` "is False"
          Left err -> error $ "Unexpected error: " ++ show err

    describe "Equals operator" $ do
      it "evaluates x == 3 correctly for matching value" $ do
        let env = Gode.Env 3 "test"
            program = Gode.If "x == 3" "match" "no-match"
        case Gode.eval program env of
          Right (result, logs) -> do
            result `shouldBe` "match"
            length logs `shouldBe` 3
            head logs `shouldContain` "is True"
          Left err -> error $ "Unexpected error: " ++ show err

      it "evaluates x == 3 correctly for non-matching value" $ do
        let env = Gode.Env 4 "test"
            program = Gode.If "x == 3" "match" "no-match"
        case Gode.eval program env of
          Right (result, logs) -> do
            result `shouldBe` "no-match"
            length logs `shouldBe` 3
            head logs `shouldContain` "is False"
          Left err -> error $ "Unexpected error: " ++ show err

    describe "Error handling" $ do
      it "handles invalid operator" $ do
        let env = Gode.Env 4 "test"
            program = Gode.If "x foo 3" "high" "low"
        case Gode.eval program env of
          Right _ -> error "Expected error"
          Left err -> err `shouldBe` Gode.InvalidOperator "foo"

      it "handles invalid number" $ do
        let env = Gode.Env 4 "test"
            program = Gode.If "x > abc" "high" "low"
        case Gode.eval program env of
          Right _ -> error "Expected error"
          Left err -> err `shouldBe` Gode.InvalidNumber "abc"

      it "handles invalid format" $ do
        let env = Gode.Env 4 "test"
            program = Gode.If "invalid format" "high" "low"
        case Gode.eval program env of
          Right _ -> error "Expected error"
          Left err -> err `shouldBe` Gode.InvalidFormat