module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Tests" $ do
    it "placeholder" $ do
      True `shouldBe` True


