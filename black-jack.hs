import Data.List
import Control.Monad

-- ## Type definition ##

data RoundResult = PlayerWin | PlayerBust | StandOff | PlayerLoose | Undefined
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

-- ## End of Type definition ##

-- setup one deck and reorder
type Deck = [Card]

createDeck :: Int -> Deck
createDeck deckCount =
    if deckCount <= 0 || deckCount > 6
    then []
    else [Card val su | val <- [Two .. Ace], su <- [Club .. Spade]] ++ createDeck (deckCount - 1)

data RoundSnapshot = RoundSnapshot { d :: Deck, dd :: Deck, pd :: Deck }
    deriving (Show)

-- random numbers [0..311] for max 6 decks à 52 cards
rand :: [Int]
rand = [287, 202, 306, 170, 15, 125, 104, 261, 117, 305, 255, 301, 246, 78, 239, 79, 98, 281, 296, 45, 210, 273, 307, 227, 41, 10, 50, 149, 252, 8, 107, 11, 53, 217, 97, 90, 247, 74, 309, 212, 184, 95, 48, 153, 81, 262, 83, 203, 272, 63, 264, 75, 187, 302, 145, 110, 300, 31, 57, 185, 249, 55, 282, 199, 100, 173, 228, 230, 24, 267, 84, 128, 62, 148, 191, 303, 133, 297, 0, 205, 198, 193, 77, 279, 36, 269, 17, 118, 85, 152, 111, 89, 28, 38, 190, 208, 216, 22, 142, 87, 113, 58, 167, 169, 288, 52, 68, 26, 195, 124, 201, 204, 70, 260, 115, 280, 292, 139, 138, 233, 5, 134, 144, 64, 225, 130, 14, 49, 164, 241, 200, 259, 285, 243, 215, 213, 82, 23, 189, 33, 30, 291, 32, 235, 155, 207, 34, 265, 127, 294, 234, 289, 299, 276, 156, 157, 37, 242, 120, 238, 222, 275, 286, 76, 9, 165, 46, 160, 310, 158, 71, 27, 122, 268, 250, 209, 119, 88, 67, 180, 103, 132, 25, 54, 197, 93, 7, 244, 168, 3, 61, 99, 214, 283, 56, 256, 179, 35, 166, 143, 254, 263, 290, 129, 51, 220, 226, 60, 20, 223, 298, 101, 80, 181, 177, 12, 172, 91, 39, 271, 311, 1, 206, 140, 163, 69, 96, 43, 29, 266, 194, 150, 109, 131, 182, 102, 188, 284, 59, 295, 231, 47, 232, 42, 137, 221, 248, 65, 253, 154, 274, 293, 192, 66, 304, 105, 178, 147, 240, 114, 73, 308, 116, 13, 161, 162, 196, 258, 270, 245, 171, 106, 175, 218, 174, 224, 278, 19, 211, 251, 92, 135, 44, 146, 141, 40, 236, 2, 6, 237, 186, 126, 121, 123, 229, 21, 16, 136, 159, 94, 112, 4, 18, 86, 183, 151, 219, 72, 176, 257, 108, 277]

shuffle :: Deck -> Deck
shuffle deck =
    map snd (sort (zip (filter (\r -> r < length deck) rand) deck))

deck :: Deck
deck = shuffle (createDeck 6)

playerInitBalance :: Int
playerInitBalance = 0

-- setup rules
draw :: Deck -> Int -> Deck
draw deck drawCount = take drawCount deck

getCardValue :: Card -> CardValue
getCardValue (Card val _) = val

getCardSuit :: Card -> Suit
getCardSuit (Card _ suit) = suit

isAce :: Card -> Bool
isAce card = getCardValue card == Ace

aceCount :: Deck -> Int
aceCount deck = length (filter isAce deck)

filterNonAces :: Deck -> Deck
filterNonAces deck = filter (not . isAce) deck

acesAsElevenCount :: Deck -> Int
acesAsElevenCount deck = (aceCount deck) - (acesAsOneCount deck)

acesAsOneCount :: Deck -> Int
acesAsOneCount deck = acesAsOneCountLoop deck 0

acesAsOneCountLoop :: Deck -> Int -> Int
acesAsOneCountLoop deck n = 
    if n < aceCount deck && sumCardValues deck n > 21
    then acesAsOneCountLoop deck (n+1)
    else n

sumCardValues :: Deck -> Int -> Int
sumCardValues deck acesAsOneCount =
    (sum $ map hardCardValue (filterNonAces deck)) + 
    (1 * acesAsOneCount) + 
    (11 * ((aceCount deck) - acesAsOneCount))

deckValue :: Deck -> Int
deckValue deck = sumCardValues deck (acesAsOneCount deck)

isBust :: Deck -> Bool
isBust deck = (deckValue deck) > 21

-- dealer stands on soft 17
doesDealerHit :: Deck -> Bool
doesDealerHit deck = (deckValue deck) < 17

-- setup game
numberOfRounds :: Int
numberOfRounds = 40

getDeck :: RoundSnapshot -> Deck
getDeck (RoundSnapshot deck _ _) = deck

getDealerDeck :: RoundSnapshot -> Deck
getDealerDeck (RoundSnapshot _ dealerDeck _) = dealerDeck;

getPlayerDeck :: RoundSnapshot -> Deck
getPlayerDeck (RoundSnapshot _ _ playerDeck) = playerDeck;

play :: Deck -> IO ()
play deck = do 
    putStrLn ("Player Balance: " ++ show playerInitBalance)
    putStrLn ("--- Playing " ++ show numberOfRounds ++ " rounds ---")
    playRoundLoop deck 0 playerInitBalance

playRoundLoop :: Deck -> Int -> Int -> IO ()
playRoundLoop deck currentRound playerBalance = 
    if currentRound < numberOfRounds
    then do 
        putStrLn ("Playing round " ++ show (currentRound + 1) ++ "/" ++ show numberOfRounds ++ " with " ++ show (length deck) ++ " cards")
        let snapshot = playRound deck
        let dealerDeck = getDealerDeck snapshot
        let playerDeck = getPlayerDeck snapshot
        let result = evaluateResult dealerDeck playerDeck

        putStrLn ("   > " ++ show result ++ ": Dealer (" ++ show (deckValue dealerDeck) ++ "), Player (" ++ show (deckValue playerDeck) ++ ")")
        playRoundLoop (getDeck snapshot) (currentRound + 1) (updateBalance dealerDeck playerDeck playerBalance)
    else do 
        putStrLn "--- End of Rounds ---"
        putStrLn ("Player Balance: " ++ show playerBalance)


updateBalance :: Deck -> Deck -> Int -> Int
updateBalance dealerDeck playerDeck balance = balance + (earnings (evaluateResult dealerDeck playerDeck))

earnings :: RoundResult -> Int
earnings roundResult 
    | roundResult == PlayerWin      = 1
    | roundResult == PlayerBust     = -1
    | roundResult == PlayerLoose    = -1
    | otherwise                     = 0

playRound :: Deck -> RoundSnapshot
playRound deck = do 
    let snapshot = playerAction (RoundSnapshot (drop 3 deck) (take 1 (drop 2 deck)) (take 2 deck))
    dealerAction snapshot

dealerAction :: RoundSnapshot -> RoundSnapshot
dealerAction snapshot = do
    let deck = getDeck snapshot
    let dealerDeck = getDealerDeck snapshot
    let playerDeck = getPlayerDeck snapshot

    if not (isBust dealerDeck) && doesDealerHit dealerDeck
    then dealerAction (RoundSnapshot (drop 1 deck) (dealerDeck ++ (take 1 deck)) playerDeck)
    else snapshot

playerAction :: RoundSnapshot -> RoundSnapshot
playerAction snapshot = do
    let deck = getDeck snapshot
    let dealerDeck = getDealerDeck snapshot
    let playerDeck = getPlayerDeck snapshot

    if not (isBust playerDeck) && doesPlayerHit dealerDeck playerDeck
    then playerAction (RoundSnapshot (drop 1 deck) dealerDeck (playerDeck ++ (take 1 deck)))
    else snapshot

doesPlayerHit :: Deck -> Deck -> Bool
doesPlayerHit dealerDeck playerDeck =
    if deckValue playerDeck < 17
    then True
    else False

evaluateResult :: Deck -> Deck -> RoundResult
evaluateResult dealerDeck playerDeck
    | deckValue playerDeck > 21                     = PlayerBust
    | deckValue dealerDeck > 21                     = PlayerWin
    | deckValue dealerDeck == deckValue playerDeck  = StandOff
    | deckValue dealerDeck > deckValue playerDeck   = PlayerLoose
    | deckValue dealerDeck < deckValue playerDeck   = PlayerWin
    | otherwise                                     = Undefined