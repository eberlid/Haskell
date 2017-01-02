module Main (main) where

import BlackJack
import Data.List

-- |Returns randomized list of numbers [0..311] (6 decks à 52 cards)
rand :: [Int]
rand = [287, 202, 306, 170, 15, 125, 104, 261, 117, 305, 255, 301, 246, 78, 239, 79, 98, 281, 296, 45, 210, 273, 307, 227, 41, 10, 50, 149, 252, 8, 107, 11, 53, 217, 97, 90, 247, 74, 309, 212, 184, 95, 48, 153, 81, 262, 83, 203, 272, 63, 264, 75, 187, 302, 145, 110, 300, 31, 57, 185, 249, 55, 282, 199, 100, 173, 228, 230, 24, 267, 84, 128, 62, 148, 191, 303, 133, 297, 0, 205, 198, 193, 77, 279, 36, 269, 17, 118, 85, 152, 111, 89, 28, 38, 190, 208, 216, 22, 142, 87, 113, 58, 167, 169, 288, 52, 68, 26, 195, 124, 201, 204, 70, 260, 115, 280, 292, 139, 138, 233, 5, 134, 144, 64, 225, 130, 14, 49, 164, 241, 200, 259, 285, 243, 215, 213, 82, 23, 189, 33, 30, 291, 32, 235, 155, 207, 34, 265, 127, 294, 234, 289, 299, 276, 156, 157, 37, 242, 120, 238, 222, 275, 286, 76, 9, 165, 46, 160, 310, 158, 71, 27, 122, 268, 250, 209, 119, 88, 67, 180, 103, 132, 25, 54, 197, 93, 7, 244, 168, 3, 61, 99, 214, 283, 56, 256, 179, 35, 166, 143, 254, 263, 290, 129, 51, 220, 226, 60, 20, 223, 298, 101, 80, 181, 177, 12, 172, 91, 39, 271, 311, 1, 206, 140, 163, 69, 96, 43, 29, 266, 194, 150, 109, 131, 182, 102, 188, 284, 59, 295, 231, 47, 232, 42, 137, 221, 248, 65, 253, 154, 274, 293, 192, 66, 304, 105, 178, 147, 240, 114, 73, 308, 116, 13, 161, 162, 196, 258, 270, 245, 171, 106, 175, 218, 174, 224, 278, 19, 211, 251, 92, 135, 44, 146, 141, 40, 236, 2, 6, 237, 186, 126, 121, 123, 229, 21, 16, 136, 159, 94, 112, 4, 18, 86, 183, 151, 219, 72, 176, 257, 108, 277]

-- |Shuffles a deck with up to 312 cards
shuffle :: Deck -> Deck
shuffle deck =
    map snd (sort (zip (filter (\r -> r < length deck) rand) deck))

-- |Returns a shuffled deck with the specified number of 52-card decks
staticShuffledDeck :: Int -> Deck
staticShuffledDeck deckCount = shuffle (createDeck deckCount)

numberOfDecks :: Int
numberOfDecks = 6

-- | Returns the number of rounds to play
initNumberOfRounds :: Int
initNumberOfRounds = 40

main :: IO ()
main = do 
    putStrLn "Lets play some Black Jack!"
    -- let deck = [Card Nine Spade, Card Five Heart, Card Nine Diamond, Card Two Heart, Card Five Club, Card Ten Heart, Card King Spade, Card Five Diamond]
    let deck = staticShuffledDeck numberOfDecks
    let tables = play deck initNumberOfRounds
    putStrLn (showTables tables)
    putStrLn "Game ended."
