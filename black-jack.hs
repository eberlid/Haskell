import Data.List
import Control.Monad
import System.Random

-- ## Type definition ##

data RoundResult = PlayerWin | PlayerBust | StandOff | PlayerLoose | PlayerBlackJack | Undefined
    deriving (Read, Show, Enum, Eq, Ord)

data Suit = Club | Diamond | Heart | Spade
    deriving (Read, Show, Enum, Eq, Ord)

data CardValue = Two | Three | Four | Five 
    | Six | Seven | Eight | Nine | Ten 
    | Jack | Queen | King | Ace
    deriving (Read, Show, Enum, Eq, Ord)

data Card = Card CardValue Suit
    deriving (Read, Show, Eq)

-- provide instance declarations for Ord and Enum
instance Ord Card where
    compare c1 c2 = compare (getCardValue c1, getCardSuit c1) (getCardValue c2, getCardSuit c2)

instance Enum Card where
    toEnum n = let (v, s) = n `divMod` 4 in Card (toEnum v) (toEnum s)
    fromEnum c = fromEnum (getCardValue c) * 4 + fromEnum (getCardSuit c)

-- |Declare 'Deck' as a synonym of array of 'Card'
type Deck = [Card]

type Hand = [Card]

-- |Declare 'Box' as a synonym of array of 'Deck'
type Box = [Hand]

data Table = Table Deck Deck [Deck]
    deriving (Show)

hardCardValue :: Card -> Int
hardCardValue c
    | getCardValue c == Two   = 2
    | getCardValue c == Three = 3
    | getCardValue c == Four  = 4
    | getCardValue c == Five  = 5
    | getCardValue c == Six   = 6
    | getCardValue c == Seven = 7
    | getCardValue c == Eight = 8
    | getCardValue c == Nine  = 9
    | getCardValue c == Ten   = 10
    | getCardValue c == Jack  = 10
    | getCardValue c == Queen = 10
    | getCardValue c == King  = 10
    | otherwise               = 0

-- |Returns the Deck of a snapshot
getDeck :: Table -> Deck
getDeck (Table deck _ _) = deck

-- |Returns the Dealer Hand of the table
getDealerHand :: Table -> Hand
getDealerHand (Table _ dealerHand _) = dealerHand

-- |Returns the Player Decks of a snapshot
getPlayerBox :: Table -> Box
getPlayerBox (Table _ _ playerBox) = playerBox

-- |Returns 'True' if the Hand is Black Jack
isBlackJack :: Hand -> Bool
isBlackJack hand = length hand == 2 && (any (\c -> getCardValue c == Ace) hand 
    && ((any (\c -> getCardValue c == Ten) hand)
        || any (\c -> getCardValue c == Jack) hand
        || any (\c -> getCardValue c == Queen) hand
        || any (\c -> getCardValue c == King) hand))

-- |Returns the Value of a card
getCardValue :: Card -> CardValue
getCardValue (Card val _) = val

-- |Returns the Suit of a card
getCardSuit :: Card -> Suit
getCardSuit (Card _ suit) = suit

-- |Returns 'True' if the hand value is > 21
isBust :: Hand -> Bool
isBust hand = (handValue hand) > 21

-- |Returns the value of the hand under the assumption that the number of hard aces must be minimal
handValue :: Hand -> Int
handValue hand = sumCardValues hand (acesAsOneCount hand)

-- |Returns the sum of the hand by counting the specified number of aces as one
sumCardValues :: Hand -> Int -> Int
sumCardValues hand acesAsOneCount =
    (sum $ map hardCardValue (filterNonAces hand)) + 
    (1 * acesAsOneCount) + 
    (11 * ((aceCount hand) - acesAsOneCount))

-- |Returns the number of Aces in the Deck
aceCount :: Hand -> Int
aceCount hand = length (filter isAce hand)

-- |Returns 'True' if the card is an Ace
isAce :: Card -> Bool
isAce card = getCardValue card == Ace

-- |Returns a filtered Deck without any aces
filterNonAces :: Hand -> Hand
filterNonAces hand = filter (not . isAce) hand

-- |Returns the number of Aces to be counted as one in the dealer hand so that the hand value is <=21
acesAsOneCount :: Hand -> Int
acesAsOneCount hand = acesAsOneCountLoop hand 0

-- |Recursive loop to determine the minimum number of Aces to be counted as 1 so that the hand value is <=21
acesAsOneCountLoop :: Hand -> Int -> Int
acesAsOneCountLoop hand n = 
    if n < aceCount hand && sumCardValues hand n > 21
    then acesAsOneCountLoop hand (n+1)
    else n

boxCardCount :: Box -> Int
boxCardCount box = foldl (+) 0 (map (\d -> length d) box)

-- |Returns a shuffled deck with the specified number of 52-card decks
staticShuffledDeck :: Int -> Deck
staticShuffledDeck deckCount = shuffle (createDeck deckCount)

-- |Shuffles a deck with up to 312 cards
shuffle :: Deck -> Deck
shuffle deck =
    map snd (sort (zip (filter (\r -> r < length deck) rand) deck))

-- |Returns randomized list of numbers [0..311] (6 decks à 52 cards)
rand :: [Int]
rand = [287, 202, 306, 170, 15, 125, 104, 261, 117, 305, 255, 301, 246, 78, 239, 79, 98, 281, 296, 45, 210, 273, 307, 227, 41, 10, 50, 149, 252, 8, 107, 11, 53, 217, 97, 90, 247, 74, 309, 212, 184, 95, 48, 153, 81, 262, 83, 203, 272, 63, 264, 75, 187, 302, 145, 110, 300, 31, 57, 185, 249, 55, 282, 199, 100, 173, 228, 230, 24, 267, 84, 128, 62, 148, 191, 303, 133, 297, 0, 205, 198, 193, 77, 279, 36, 269, 17, 118, 85, 152, 111, 89, 28, 38, 190, 208, 216, 22, 142, 87, 113, 58, 167, 169, 288, 52, 68, 26, 195, 124, 201, 204, 70, 260, 115, 280, 292, 139, 138, 233, 5, 134, 144, 64, 225, 130, 14, 49, 164, 241, 200, 259, 285, 243, 215, 213, 82, 23, 189, 33, 30, 291, 32, 235, 155, 207, 34, 265, 127, 294, 234, 289, 299, 276, 156, 157, 37, 242, 120, 238, 222, 275, 286, 76, 9, 165, 46, 160, 310, 158, 71, 27, 122, 268, 250, 209, 119, 88, 67, 180, 103, 132, 25, 54, 197, 93, 7, 244, 168, 3, 61, 99, 214, 283, 56, 256, 179, 35, 166, 143, 254, 263, 290, 129, 51, 220, 226, 60, 20, 223, 298, 101, 80, 181, 177, 12, 172, 91, 39, 271, 311, 1, 206, 140, 163, 69, 96, 43, 29, 266, 194, 150, 109, 131, 182, 102, 188, 284, 59, 295, 231, 47, 232, 42, 137, 221, 248, 65, 253, 154, 274, 293, 192, 66, 304, 105, 178, 147, 240, 114, 73, 308, 116, 13, 161, 162, 196, 258, 270, 245, 171, 106, 175, 218, 174, 224, 278, 19, 211, 251, 92, 135, 44, 146, 141, 40, 236, 2, 6, 237, 186, 126, 121, 123, 229, 21, 16, 136, 159, 94, 112, 4, 18, 86, 183, 151, 219, 72, 176, 257, 108, 277]

-- |Returns the specified number of 52-card decks as on ordered list
createDeck :: Int -> Deck
createDeck deckCount =
    if deckCount <= 0 || deckCount > 6
    then []
    else [Card val su | val <- [Two .. Ace], su <- [Club .. Spade]] ++ createDeck (deckCount - 1)

-- |Returns the number of rounds to play
numberOfRounds :: Int
numberOfRounds = 10

numberOfDecks :: Int
numberOfDecks = 6

play :: IO ()
play = do 
    let deck = (staticShuffledDeck numberOfDecks)
    putStrLn (show (take 21 (drop 28 deck)))
    playLoop deck numberOfRounds 0

playLoop :: Deck -> Int -> Int -> IO ()
playLoop deck numberOfRounds currentRound = do 
    if (currentRound <= numberOfRounds)
    then do 
        putStrLn ("Playing loop (" ++ show currentRound ++ "/" ++ show numberOfRounds ++ ") with " ++ show (length deck) ++ " cards (" ++ show ((numberOfDecks * 52) - length deck) ++ " cards played).")

        let table = playBox (drop 3 deck) [(head (drop 1 deck))] [head deck, head (drop 2 deck)]
        putStrLn ("  > Dealer Deck (" ++ show (handValue (getDealerHand table)) ++ "): " ++ show (getDealerHand table))
        mapM (\ d -> putStrLn ("  > Player Deck (" ++ show (handValue d) ++ "): " ++ show d)) (getPlayerBox table)
        playLoop (getDeck table) numberOfRounds (currentRound + 1)
    else putStrLn ("End of Game")

playBox :: Deck -> Hand -> Hand -> Table
playBox deck dealerHand playerHand = do
    let playerBox = playHand deck dealerHand playerHand
    let dealedCardCount = boxCardCount playerBox - 2
    Table (drop (dealedCardCount) deck) dealerHand playerBox

playHand :: Deck -> Hand -> Hand -> Box
playHand deck dealerHand playerHand 
    | canSplit playerHand = do
        let left = playHand deck dealerHand [head playerHand]
        let right = playHand (drop (boxCardCount left - 1) deck) dealerHand [head (tail playerHand)]
        left ++ right
    | canHit playerHand && voteHit dealerHand playerHand = foldl (++) [] (map (\d -> playHand (tail deck) dealerHand d) [playerHand ++ [head deck]])
    | otherwise = [playerHand]

canSplit :: Hand -> Bool
canSplit hand = length hand == 2 && handValue (take 1 hand) == handValue (drop 1 hand)

voteSplit :: Hand -> Hand -> Bool
voteSplit dealerHand playerHand = True

canHit :: Hand -> Bool
canHit hand = handValue hand <= 21

voteHit :: Hand -> Hand -> Bool
voteHit dealerHand playerHand = handValue playerHand < 17
