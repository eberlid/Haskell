module BlackJackSpec (spec) where

import BlackJack (foo)
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do 
    describe "blackjack" $ do
        it "returns the unit value" $ do
            foo `shouldBe` ()

        prop "equals the unit value" $
            \ x -> foo == x

        it "returns the first element on a list" $ do
            head [23..] `shouldBe` (22 :: Int)