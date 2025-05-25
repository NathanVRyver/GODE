import qualified Gode.Core as Gode
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "GODE" $ do
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