module BlackJackSpec where

import BlackJack
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "dealer" $ do
        it "stands on hard seventeen" $ getCards
            (dealerAction []
                (Hand [Card Ten Club, Card Seven Spade] False False))
            `shouldBe` [Card Ten Club, Card Seven Spade]

        it "hits on hard sixteen" $ getCards
            (dealerAction [Card Eight Club]
                (Hand [Card Ten Club, Card Six Spade] False False))
            `shouldBe` [Card Ten Club, Card Six Spade, Card Eight Club]

        it "stands on soft seventeen" $ getCards
            (dealerAction []
                (Hand [Card Ace Club, Card Six Spade] False False))
            `shouldBe` [Card Ace Club, Card Six Spade]

        it "hits on soft sixteen" $ getCards
            (dealerAction [Card Four Club]
                (Hand [Card Ace Club, Card Five Spade] False False))
            `shouldBe` [Card Ace Club, Card Five Spade, Card Four Club]

        it "hits on soft bust" $ getCards
            (dealerAction [Card Six Club, Card Two Heart, Card Three Diamond]
                (Hand [Card Ace Club, Card Five Spade] False False))
            `shouldBe` [Card Ace Club, Card Five Spade, Card Six Club, Card Two Heart, Card Three Diamond]
            