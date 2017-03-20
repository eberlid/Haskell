module BlackJackSpec where

import BlackJack
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "BlackJack" $ do
    describe "Dealer" $ do
        it "stands on hard seventeen" $
            dealerAction [] (freshHand [Card Ten Club, Card Seven Spade])
            `shouldBe` freshHand [Card Ten Club, Card Seven Spade]

        it "hits on hard sixteen" $
            dealerAction [Card Eight Club] (freshHand [Card Ten Club, Card Six Spade])
            `shouldBe` freshHand [Card Ten Club, Card Six Spade, Card Eight Club] 

        it "stands on soft seventeen" $
            dealerAction [] (freshHand [Card Ace Club, Card Six Spade])
            `shouldBe` freshHand [Card Ace Club, Card Six Spade]

        it "hits on soft sixteen" $
            dealerAction [Card Four Club] (freshHand [Card Ace Club, Card Five Spade])
            `shouldBe` freshHand [Card Ace Club, Card Five Spade, Card Four Club]

        it "hits on soft bust" $
            dealerAction [Card Six Club, Card Two Heart, Card Three Diamond] (freshHand [Card Ace Club, Card Five Spade])
            `shouldBe` freshHand [Card Ace Club, Card Five Spade, Card Six Club, Card Two Heart, Card Three Diamond]
    
    describe "Split" $ do
        it "when equal value and split count is zero is allowed " $
            canSplit (Hand [Card Six Club, Card Six Spade] False False) 0
            `shouldBe` True

        it "when equal value and split count is one is allowed " $
            canSplit (Hand [Card Six Club, Card Six Spade] False False) 1
            `shouldBe` True

        it "when equal value and split count is two is allowed " $
            canSplit (Hand [Card Six Club, Card Six Spade] False False) 2
            `shouldBe` True

        it "when equal value and split count is three is not allowed " $
            canSplit (Hand [Card Six Club, Card Six Spade] False False) 3
            `shouldBe` False

        it "when not equal value and split count is zero is not allowed " $
            canSplit (Hand [Card Six Club, Card Ten Spade] False False) 0
            `shouldBe` False

        it "when not equal value and split count is three is not allowed " $
            canSplit (Hand [Card Six Club, Card Ten Spade] False False) 3
            `shouldBe` False