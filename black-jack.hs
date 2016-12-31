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

-- |Declare 'Deck' as a synonym of array of 'Card'
type Deck = [Card]

-- |Declare 'Box' as a synonym of array of 'Deck'
type Box = [Deck]

-- |Declare 'RoundSnapshot' with Deck, Dealer Deck, Player Deck array and player balance
data RoundSnapshot = RoundSnapshot Deck Deck Box Int
    deriving (Show)

-- |Declare 'PlayerActionSnapshot' with Deck, Dealer Deck, Player Deck, player balance and the bet
data PlayerActionSnapshot = PlayerActionSnapshot Deck Deck Deck Int Int
    deriving (Show)

-- |Returns the specified number of 52-card decks as on ordered list
createDeck :: Int -> Deck
createDeck deckCount =
    if deckCount <= 0 || deckCount > 6
    then []
    else [Card val su | val <- [Two .. Ace], su <- [Club .. Spade]] ++ createDeck (deckCount - 1)

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

-- |Returns the initial balance of the player
playerInitBalance :: Int
playerInitBalance = 0

-- |Returns the Value of a card
getCardValue :: Card -> CardValue
getCardValue (Card val _) = val

-- |Returns the Suit of a card
getCardSuit :: Card -> Suit
getCardSuit (Card _ suit) = suit

-- |Returns 'True' if the card is an Ace
isAce :: Card -> Bool
isAce card = getCardValue card == Ace

-- |Returns the number of Aces in the Deck
aceCount :: Deck -> Int
aceCount deck = length (filter isAce deck)

-- |Returns 'True' if the Deck is Black Jack
isBlackJack :: Deck -> Bool
isBlackJack deck = length deck == 2 && (any (\c -> getCardValue c == Ace) deck 
    && ((any (\c -> getCardValue c == Ten) deck)
        || any (\c -> getCardValue c == Jack) deck
        || any (\c -> getCardValue c == Queen) deck
        || any (\c -> getCardValue c == King) deck))

-- |Returns a filtered Deck without any aces
filterNonAces :: Deck -> Deck
filterNonAces deck = filter (not . isAce) deck

-- |
acesAsElevenCount :: Deck -> Int
acesAsElevenCount deck = (aceCount deck) - (acesAsOneCount deck)

-- |Returns the number of Aces to be counted as one in the dealer deck so that the deck value is <=21
acesAsOneCount :: Deck -> Int
acesAsOneCount deck = acesAsOneCountLoop deck 0

-- |Recursive loop to determine the minimum number of Aces to be counted as 1 so that the deck value is <=21
acesAsOneCountLoop :: Deck -> Int -> Int
acesAsOneCountLoop deck n = 
    if n < aceCount deck && sumCardValues deck n > 21
    then acesAsOneCountLoop deck (n+1)
    else n

-- |Returns the sum of the deck by counting the specified number of aces as one
sumCardValues :: Deck -> Int -> Int
sumCardValues deck acesAsOneCount =
    (sum $ map hardCardValue (filterNonAces deck)) + 
    (1 * acesAsOneCount) + 
    (11 * ((aceCount deck) - acesAsOneCount))

-- |Returns the value of the deck under the assumption that the number of hard aces must be minimal
deckValue :: Deck -> Int
deckValue deck = sumCardValues deck (acesAsOneCount deck)

-- |Returns 'True' if the deck value is > 21
isBust :: Deck -> Bool
isBust deck = (deckValue deck) > 21

-- |Returns 'True' if the deck value is < 17
doesDealerHit :: Deck -> Bool
doesDealerHit deck = (deckValue deck) < 17

-- |Returns the number of rounds to play
numberOfRounds :: Int
numberOfRounds = 15

-- |Returns the Balance of a snapshot
getBalance :: RoundSnapshot -> Int
getBalance (RoundSnapshot _ _ _ balance) = balance

-- |Returns the Deck of a snapshot
getDeck :: RoundSnapshot -> Deck
getDeck (RoundSnapshot deck _ _ _) = deck

-- |Returns the Dealer Deck of a snapshot
getDealerDeck :: RoundSnapshot -> Deck
getDealerDeck (RoundSnapshot _ dealerDeck _ _) = dealerDeck

-- |Returns the Player Decks of a snapshot
getPlayerBox :: RoundSnapshot -> Box
getPlayerBox (RoundSnapshot _ _ playerBox _) = playerBox

getPlayerBalance :: RoundSnapshot -> Int
getPlayerBalance (RoundSnapshot _ _ _ balance) = balance

-- |Returns the Deck of a snapshot
getDeck2 :: PlayerActionSnapshot -> Deck
getDeck2 (PlayerActionSnapshot deck _ _ _ _) = deck

-- |Returns the Player Deck of a snapshot
getPlayerDeck :: PlayerActionSnapshot -> Deck
getPlayerDeck (PlayerActionSnapshot _ _ playerDeck _ _) = playerDeck

-- |Returns the Player Deck of a snapshot
getDealerDeck2 :: PlayerActionSnapshot -> Deck
getDealerDeck2 (PlayerActionSnapshot _ dealerDeck _ _ _) = dealerDeck

-- |Returns the Balance of a snapshot
getBalance2 :: PlayerActionSnapshot -> Int
getBalance2 (PlayerActionSnapshot _ _ _ balance _) = balance

-- |Returns the bet
getBet :: PlayerActionSnapshot -> Int
getBet (PlayerActionSnapshot _ _ _ _ bet) = bet

-- |Plays the game
play :: IO ()
play = do 
    putStrLn ("Player Balance: " ++ show playerInitBalance)
    putStrLn ("--- Playing " ++ show numberOfRounds ++ " rounds ---")

    let deck = staticShuffledDeck 6
    putStrLn (show (take 10 (drop 83 deck)))
    playRoundLoop deck 0 playerInitBalance

-- |Recursive loop to play one round
playRoundLoop :: Deck -> Int -> Int -> IO ()
playRoundLoop deck currentRound playerBalance = 
    if currentRound < numberOfRounds
    then do 
        putStrLn ("Playing round " ++ show (currentRound + 1) ++ "/" ++ show numberOfRounds ++ " with " ++ show (length deck) ++ " cards")
        let snapshot = playRound deck playerBalance
        let dealerDeck = getDealerDeck snapshot
        let playerBox = getPlayerBox snapshot
        mapM (\d -> putStrLn ("  > Player Deck (" ++ show (deckValue d) ++ "): " ++ show d)) playerBox
        putStrLn ("  > Dealer Deck (" ++ show (deckValue dealerDeck) ++ "): " ++ show dealerDeck)
        -- let result = evaluateResult dealerDeck playerDeck

        --putStrLn ("   > " ++ show result ++ ": Dealer (" ++ show (deckValue dealerDeck) ++ "), Player (" ++ show (deckValue playerDeck) ++ ")")
        playRoundLoop (getDeck snapshot) (currentRound + 1) (getBalance snapshot)
    else do 
        putStrLn "--- End of Rounds ---"
        putStrLn ("Player Balance: " ++ show playerBalance)

-- |Plays one round and returns the round snapshot
playRound :: Deck -> Int -> RoundSnapshot
playRound deck balance = do
    let init = initRound deck balance
    let playerActions = playerAction init
    let playerDecks = map (\s -> getPlayerDeck s) playerActions
    let newbalance = (balance - foldl (+) 0 (map (\a -> getBet a) playerActions))
    
    let (newDeck, newDealerDeck) = dealerAction (getDeck2 (head (reverse playerActions))) (getDealerDeck2 init)

    RoundSnapshot  newDeck newDealerDeck playerDecks newbalance

-- |Returns initial round situation where player can act upon
initRound :: Deck -> Int -> PlayerActionSnapshot
initRound deck balance = do
    let playerDeck = [head deck, head (drop 2 deck)]
    let dealerDeck = take 1 (drop 1 deck)
    let residualDeck = drop 3 deck
    PlayerActionSnapshot residualDeck dealerDeck playerDeck balance 1

playerAction :: PlayerActionSnapshot -> [PlayerActionSnapshot]
playerAction snapshot = do
    let deck = getDeck2 snapshot
    let dealerDeck = getDealerDeck2 snapshot
    let playerDeck = getPlayerDeck snapshot

    if isBust playerDeck
    then [snapshot]
    else
        if canSplit playerDeck (getBalance2 snapshot) (getBet snapshot) && voteSplit snapshot
        then do
            let splittedDeck = splitDeck playerDeck
            let fstDeckActions = playerAction (PlayerActionSnapshot deck dealerDeck (fst splittedDeck) (getBalance2 snapshot - getBet snapshot) (getBet snapshot))
            let sndDeckActions = playerAction (PlayerActionSnapshot ((getDeck2 (head fstDeckActions))) dealerDeck (snd splittedDeck) (getBalance2 snapshot - foldl (+) 0 (map (\a -> getBet a) fstDeckActions)) (getBet snapshot))
            fstDeckActions ++ sndDeckActions
        else
            if doesPlayerHit dealerDeck playerDeck
            then playerAction (PlayerActionSnapshot (tail deck) dealerDeck (playerDeck ++ [head deck]) (getBalance2 snapshot) (getBet snapshot))
            else [snapshot]

splitDeck :: Deck -> (Deck, Deck)
splitDeck deck = (take 1 deck, drop 1 deck)

canSplit :: Deck -> Int -> Int -> Bool
canSplit deck balance bet = length deck == 2 && deckValue (take 1 deck) == deckValue (drop 1 deck)

voteSplit :: PlayerActionSnapshot -> Bool
voteSplit snapshot = True

-- updateBalance :: Deck -> Deck -> Int -> Int
-- updateBalance dealerDeck playerDeck balance = balance + (earnings (evaluateResult dealerDeck playerDeck))
-- 
-- earnings :: RoundResult -> Int
-- earnings roundResult 
--     | roundResult == PlayerWin      = 1
--     | roundResult == PlayerBust     = -1
--     | roundResult == PlayerLoose    = -1
--     | otherwise                     = 0
-- 
-- 
dealerAction :: Deck -> Deck -> (Deck, Deck)
dealerAction deck dealerDeck = do
 
    if not (isBust dealerDeck) && not (isBlackJack dealerDeck) && doesDealerHit dealerDeck
    then dealerAction (drop 1 deck) (dealerDeck ++ [head deck])
    else (deck, dealerDeck)

doesPlayerHit :: Deck -> Deck -> Bool
doesPlayerHit dealerDeck playerDeck =
    if deckValue playerDeck < 17
    then True
    else False
-- 
-- evaluateResult :: Deck -> Deck -> RoundResult
-- evaluateResult dealerDeck playerDeck
--     | deckValue playerDeck > 21                                 = PlayerBust
--     | deckValue dealerDeck > 21                                 = PlayerWin
--     | deckValue dealerDeck == deckValue playerDeck              = StandOff
--     | deckValue dealerDeck > deckValue playerDeck               = PlayerLoose
--     | deckValue dealerDeck < deckValue playerDeck               = PlayerWin
--     | otherwise                                                 = Undefined

