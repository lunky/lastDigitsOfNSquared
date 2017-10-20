import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "green" $ do
    it "1 should be 1" $ do
      green 1  `shouldBe` 1 
    it "green 2  `shouldBe` 5" $ do 
      green 2  `shouldBe` 5
    it " shouldbe 7" $ do
      green 3  `shouldBe` 6
    it "shouldbe 25" $ do
      green 4  `shouldBe` 25
--    it "12 should be 2890625" $ do
--      green 12  `shouldBe` 2890625
--    it "13 should be 7109376" $ do
--      green 13  `shouldBe` 7109376 
    describe "seriesGenerator" $ do
      it "should have the first 5 right" $ do
        (take 5 $ seriesGenerator) `shouldBe` [1, 5, 6, 25, 76]
--    it "100 should be 6188999442576576769103890995893380022607743740081787109376" $ do
--      green 100  `shouldBe` 6188999442576576769103890995893380022607743740081787109376618899944257657676910389099589338002260774374008178710937661889994425765767691038909958933800226077437400817871093766188999442576576769103890995893380022607743740081787109376
