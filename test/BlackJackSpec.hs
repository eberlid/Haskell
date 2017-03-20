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
            canSplit (freshHand [Card Six Club, Card Six Spade]) 0
            `shouldBe` True

        it "when equal value and split count is one is allowed " $
            canSplit (freshHand [Card Six Club, Card Six Spade]) 1
            `shouldBe` True

        it "when equal value and split count is two is allowed " $
            canSplit (freshHand [Card Six Club, Card Six Spade]) 2
            `shouldBe` True

        it "when equal value and split count is three is not allowed " $
            canSplit (freshHand [Card Six Club, Card Six Spade]) 3
            `shouldBe` False

        it "when not equal value and split count is zero is not allowed " $
            canSplit (freshHand [Card Six Club, Card Ten Spade]) 0
            `shouldBe` False

        it "when not equal value and split count is three is not allowed " $
            canSplit (freshHand [Card Six Club, Card Ten Spade]) 3
            `shouldBe` False

    describe "Double" $ do
        it "when card count is two then double is allowed" $
            canDouble (freshHand [Card Two Club, Card Ten Spade])
            `shouldBe` True

        it "when card count is not two then double is not allowed" $
            canDouble (freshHand [Card Two Club, Card Ten Spade, Card Five Heart])
            `shouldBe` False

    describe "Hand Value" $ do
        it "when fresh hand then ace counts 11 on soft value" $
            handValue (freshHand [Card Ace Club, Card Three Spade])
            `shouldBe` 14

        it "when fresh hand then ace counts 1 on hard value" $
            handValue (Hand [Card Ace Club, Card Three Spade, Card Nine Diamond] False False)
            `shouldBe` 13

        it "when doubled hand then ace counts 11" $
            handValue (Hand [Card Ace Club, Card Three Spade, Card Nine Diamond] False True)
            `shouldBe` 23

    describe "Black Jack" $ do
        it "when splitted hand then is not black jack" $
            isBlackJack (Hand [Card Ten Club, Card Ace Diamond] True False)
            `shouldBe` False

        it "when fresh hand then is black jack" $
            isBlackJack (Hand [Card Ten Club, Card Ace Diamond] False False)
            `shouldBe` True