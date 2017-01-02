module BlackJackSpec (spec) where

import BlackJack
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "dealer" $ do
        it "stands on hard seventeen" $ do
            getCards (dealerAction [] (Hand [Card Ten Club, Card Seven Spade] False False)) 
                `shouldBe` ([Card Ten Club, Card Seven Spade])

        it "hits on hard sixteen" $ do
            getCards (dealerAction [Card Eight Club] (Hand [Card Ten Club, Card Six Spade] False False)) 
                `shouldBe` ([Card Ten Club, Card Six Spade, Card Eight Club])

        it "stands on soft seventeen" $ do
            getCards (dealerAction [] (Hand [Card Ace Club, Card Six Spade] False False)) 
                `shouldBe` ([Card Ace Club, Card Six Spade])

        it "hits on soft sixteen" $ do
            getCards (dealerAction [Card Four Club] (Hand [Card Ace Club, Card Five Spade] False False)) 
                `shouldBe` ([Card Ace Club, Card Five Spade, Card Four Club])

        it "hits on soft bust" $ do
            getCards (dealerAction [Card Six Club, Card Two Heart, Card Three Diamond] (Hand [Card Ace Club, Card Five Spade] False False)) 
                `shouldBe` ([Card Ace Club, Card Five Spade, Card Six Club, Card Two Heart, Card Three Diamond])