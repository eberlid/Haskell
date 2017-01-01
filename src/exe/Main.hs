module Main (main) where

import BlackJack

-- | Returns the number of rounds to play
initNumberOfRounds :: Int
initNumberOfRounds = 40

main :: IO ()
main = do 
    putStrLn "Lets play some Black Jack!"
    let tables = play initNumberOfRounds
    putStrLn (showTables tables)
    putStrLn "Game ended."
