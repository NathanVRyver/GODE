import qualified Gode.Core as Gode
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "GODE" $ do
    describe "Greater than operator" $ do
      it "evaluates x > 3 correctly for value above threshold" $ do
        case Gode.evalSimple "4" ">" "3" "high" "low" of
          Right (result, logs) -> do
            result `shouldBe` "high"
            length logs `shouldBe` 3
            head logs `shouldContain` "Variable 'x' = 4"
            logs !! 1 `shouldContain` "Comparison: 4 > 3 = True"
            logs !! 2 `shouldContain` "Decision: condition is True"
          Left err -> error $ "Unexpected error: " ++ show err

      it "evaluates x > 3 correctly for value below threshold" $ do
        case Gode.evalSimple "2" ">" "3" "high" "low" of
          Right (result, logs) -> do
            result `shouldBe` "low"
            length logs `shouldBe` 3
            head logs `shouldContain` "Variable 'x' = 2"
            logs !! 1 `shouldContain` "Comparison: 2 > 3 = False"
            logs !! 2 `shouldContain` "Decision: condition is False"
          Left err -> error $ "Unexpected error: " ++ show err

    describe "Greater than or equal operator" $ do
      it "evaluates x >= 3 correctly for equal value" $ do
        case Gode.evalSimple "3" ">=" "3" "high" "low" of
          Right (result, logs) -> do
            result `shouldBe` "high"
            length logs `shouldBe` 3
            logs !! 1 `shouldContain` "Comparison: 3 >= 3 = True"
          Left err -> error $ "Unexpected error: " ++ show err

      it "evaluates x >= 3 correctly for value below threshold" $ do
        case Gode.evalSimple "2" ">=" "3" "high" "low" of
          Right (result, logs) -> do
            result `shouldBe` "low"
            length logs `shouldBe` 3
            logs !! 1 `shouldContain` "Comparison: 2 >= 3 = False"
          Left err -> error $ "Unexpected error: " ++ show err

    describe "Less than operator" $ do
      it "evaluates x < 3 correctly for value below threshold" $ do
        case Gode.evalSimple "2" "<" "3" "low" "high" of
          Right (result, logs) -> do
            result `shouldBe` "low"
            length logs `shouldBe` 3
            logs !! 1 `shouldContain` "Comparison: 2 < 3 = True"
          Left err -> error $ "Unexpected error: " ++ show err

      it "evaluates x < 3 correctly for value above threshold" $ do
        case Gode.evalSimple "4" "<" "3" "low" "high" of
          Right (result, logs) -> do
            result `shouldBe` "high"
            length logs `shouldBe` 3
            logs !! 1 `shouldContain` "Comparison: 4 < 3 = False"
          Left err -> error $ "Unexpected error: " ++ show err

    describe "Less than or equal operator" $ do
      it "evaluates x <= 3 correctly for equal value" $ do
        case Gode.evalSimple "3" "<=" "3" "low" "high" of
          Right (result, logs) -> do
            result `shouldBe` "low"
            length logs `shouldBe` 3
            logs !! 1 `shouldContain` "Comparison: 3 <= 3 = True"
          Left err -> error $ "Unexpected error: " ++ show err

      it "evaluates x <= 3 correctly for value above threshold" $ do
        case Gode.evalSimple "4" "<=" "3" "low" "high" of
          Right (result, logs) -> do
            result `shouldBe` "high"
            length logs `shouldBe` 3
            logs !! 1 `shouldContain` "Comparison: 4 <= 3 = False"
          Left err -> error $ "Unexpected error: " ++ show err

    describe "Equals operator" $ do
      it "evaluates x == 3 correctly for equal value" $ do
        case Gode.evalSimple "3" "==" "3" "match" "no-match" of
          Right (result, logs) -> do
            result `shouldBe` "match"
            length logs `shouldBe` 3
            logs !! 1 `shouldContain` "Comparison: 3 == 3 = True"
          Left err -> error $ "Unexpected error: " ++ show err

      it "evaluates x == 3 correctly for different value" $ do
        case Gode.evalSimple "4" "==" "3" "match" "no-match" of
          Right (result, logs) -> do
            result `shouldBe` "no-match"
            length logs `shouldBe` 3
            logs !! 1 `shouldContain` "Comparison: 4 == 3 = False"
          Left err -> error $ "Unexpected error: " ++ show err

    describe "Error handling" $ do
      it "handles invalid operator" $ do
        case Gode.evalSimple "4" "foo" "3" "high" "low" of
          Right _ -> error "Expected error"
          Left err -> err `shouldBe` Gode.InvalidOperator "foo"

      it "handles invalid first number" $ do
        case Gode.evalSimple "abc" ">" "3" "high" "low" of
          Right _ -> error "Expected error"
          Left err -> err `shouldBe` Gode.InvalidNumber "abc"

      it "handles invalid second number" $ do
        case Gode.evalSimple "4" ">" "xyz" "high" "low" of
          Right _ -> error "Expected error"
          Left err -> err `shouldBe` Gode.InvalidNumber "xyz"