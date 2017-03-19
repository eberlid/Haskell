module BlackJackSpec where

import BlackJack
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "BlackJack" $
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
            