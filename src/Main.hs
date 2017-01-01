module Main where

import Data.List

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
    deriving (Eq)

instance Show Card where
    show c = show (getCardValue c) ++ " of " ++ show (getCardSuit c)

-- provide instance declarations for Ord and Enum
instance Ord Card where
    compare c1 c2 = compare (getCardValue c1, getCardSuit c1) (getCardValue c2, getCardSuit c2)

instance Enum Card where
    toEnum n = let (v, s) = n `divMod` 4 in Card (toEnum v) (toEnum s)
    fromEnum c = fromEnum (getCardValue c) * 4 + fromEnum (getCardSuit c)

-- |Declare 'Deck' as a synonym of array of 'Card'
type Deck = [Card]

data Hand = Hand [Card] Bool Bool
    deriving (Show)

data PlayerDeck = PlayerDeck Hand Float
    deriving (Show)

-- |Declare 'Box' as a synonym of array of 'Deck'
data Box = Box [PlayerDeck] Int
    deriving (Show)

data Table = Table Deck Hand Box
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

getPlayerHand :: PlayerDeck -> Hand
getPlayerHand (PlayerDeck hand _ ) = hand

getBet :: PlayerDeck -> Float
getBet (PlayerDeck _ b) = b

getCards :: Hand -> [Card]
getCards (Hand cards _ _) = cards

getSplitCount :: Box -> Int
getSplitCount (Box _ c) = c

getPlayerDecks :: Box -> [PlayerDeck]
getPlayerDecks (Box p _) = p

isSplitted :: PlayerDeck -> Bool
isSplitted playerDeck = isSplittedHand (getPlayerHand playerDeck)

isSplittedHand :: Hand -> Bool
isSplittedHand (Hand _ split _) = split

isDoubled :: Hand -> Bool
isDoubled (Hand _ _ double) = double

-- |Returns the Player Decks of a snapshot
getPlayerBox :: Table -> Box
getPlayerBox (Table _ _ playerBox) = playerBox

-- |Returns 'True' if the Hand is Black Jack
isBlackJack :: Hand -> Bool
isBlackJack hand = not (isSplittedHand hand) && 
    length (getCards hand) == 2 && (any (\c -> getCardValue c == Ace) (getCards hand)
        && (any (\ c -> getCardValue c == Ten) (getCards hand) ||
            any (\ c -> getCardValue c == Jack) (getCards hand) ||
                any (\ c -> getCardValue c == Queen) (getCards hand) ||
                    any (\ c -> getCardValue c == King) (getCards hand)))

isNotBlackJack :: Hand -> Bool
isNotBlackJack hand = not (isBlackJack hand)

-- |Returns the Value of a card
getCardValue :: Card -> CardValue
getCardValue (Card val _) = val

-- |Returns the Suit of a card
getCardSuit :: Card -> Suit
getCardSuit (Card _ suit) = suit

-- |Returns 'True' if the hand value is > 21
isBust :: [Card] -> Bool
isBust cards = handValue cards > 21

isNotBust :: [Card] -> Bool
isNotBust cards = not (isBust cards)

-- |Returns the value of the hand under the assumption that the number of hard aces must be minimal
handValue :: [Card] -> Int
handValue cards = sumCardValues cards (acesAsOneCount cards)

-- |Returns the sum of the hand by counting the specified number of aces as one
sumCardValues :: [Card] -> Int -> Int
sumCardValues cards acesOsAne =
    sum (map hardCardValue (filterNonAces cards)) +
        (1 * acesOsAne) + 
    (11 * (aceCount cards - acesOsAne))

-- |Returns the number of Aces in the Deck
aceCount :: [Card] -> Int
aceCount cards = length (filter isAce cards)

-- |Returns 'True' if the card is an Ace
isAce :: Card -> Bool
isAce card = getCardValue card == Ace

-- |Returns a filtered Deck without any aces
filterNonAces :: [Card] -> [Card]
filterNonAces = filter (not . isAce)

-- |Returns the number of Aces to be counted as one in the dealer hand so that the hand value is <=21
acesAsOneCount :: [Card] -> Int
acesAsOneCount cards = acesAsOneCountLoop cards 0

-- |Recursive loop to determine the minimum number of Aces to be counted as 1 so that the hand value is <=21
acesAsOneCountLoop :: [Card] -> Int -> Int
acesAsOneCountLoop cards n = 
    if n < aceCount cards && sumCardValues cards n > 21
    then acesAsOneCountLoop cards (n+1)
    else n

boxCardCount :: Box -> Int
boxCardCount box = sum (map (length . getCards . getPlayerHand) (getPlayerDecks box))

boxHandCount :: Box -> Int
boxHandCount box = length (getPlayerDecks box)

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

bet :: Float
bet = 1.0

-- |Returns the number of rounds to play
initNumberOfRounds :: Int
initNumberOfRounds = 40

numberOfDecks :: Int
numberOfDecks = 6

main :: IO ()
main = play

play :: IO ()
play = do 
    let deck = staticShuffledDeck numberOfDecks
    printDeck deck
    playLoop deck initNumberOfRounds 0 0

playLoop :: Deck -> Int -> Int -> Float -> IO ()
playLoop deck numberOfRounds roundsPlayed balance =
    if roundsPlayed < numberOfRounds
    then do 
        putStrLn ("Playing loop (" ++ show (roundsPlayed + 1) ++ "/" ++ show numberOfRounds ++ ") with " ++ show (length deck) ++ " cards (" ++ show ((numberOfDecks * 52) - length deck) ++ " cards played).")

        let table = playBox (drop 3 deck) (Hand [deck !! 1] False False) (PlayerDeck (Hand [head deck, deck !! 2] False False) bet)
        let earnings = evaluateEarnings table

        printDealerDeck (getDealerHand table)
        printPlayerBox (getPlayerBox table)

        putStrLn ("    > Yield: " ++ (if earnings > 0 then "+" else "") ++ show earnings)
        putStrLn ("    > Balance: " ++ show (balance + earnings))
        playLoop (getDeck table) numberOfRounds (roundsPlayed + 1) (balance + earnings)
    else putStrLn "End of Game"

printDeck :: Deck -> IO ()
printDeck deck = putStrLn (concatMap (\ c -> "[" ++ show (fst c + 1) ++ "]: " ++ show (snd c) ++ "\r\n") (zip [0..length deck] deck))

printDealerDeck :: Hand -> IO ()
printDealerDeck hand = putStrLn ("    > Dealer Deck (" ++ (if isBlackJack hand then "BJ" else show (handValue (getCards hand))) ++ "): " ++ show (getCards hand))

printPlayerBox :: Box -> IO ()
printPlayerBox box = do
    putStrLn ("    > Box SplitCount: " ++ show (getSplitCount box))
    mapM_ printPlayerDeck (getPlayerDecks box)


printPlayerDeck :: PlayerDeck -> IO ()
printPlayerDeck playerDeck = putStrLn
    ("        > Player Deck [" ++ handMode (getPlayerHand playerDeck) ++ "] (" ++
        (if isBlackJack (getPlayerHand playerDeck) then "BJ" else show (handValue (getCards (getPlayerHand playerDeck))))
            ++ "): " ++ show (getCards (getPlayerHand playerDeck)))

handMode :: Hand -> String
handMode hand = (if isSplittedHand hand then "S" else "-") ++ (if isDoubled hand then "D" else "-")

playBox :: Deck -> Hand -> PlayerDeck -> Table
playBox deck dealerHand playerDeck = do
    let playerBox = playHand deck 0 dealerHand playerDeck
    let playerBoxDealedCardCount = boxCardCount playerBox - 2

    let finalDealerHand = dealerAction (drop playerBoxDealedCardCount deck) dealerHand
    let dealerHandDealedCardCount = length (getCards finalDealerHand) - 1
    
    Table (drop (playerBoxDealedCardCount + dealerHandDealedCardCount) deck) finalDealerHand playerBox

dealerAction :: Deck -> Hand -> Hand
dealerAction deck dealerHand 
    | handValue (getCards dealerHand) < 17 = dealerAction (tail deck) (Hand (getCards dealerHand ++ [head deck]) False False)
    | otherwise = dealerHand

playHand :: Deck -> Int -> Hand -> PlayerDeck -> Box
playHand deck splitCount dealerHand playerDeck
    | canSplit (getPlayerHand playerDeck) splitCount = do
        let left = playHand deck (splitCount + 1) dealerHand (PlayerDeck (Hand [head (getCards (getPlayerHand playerDeck))] True False) (getBet playerDeck))
        let right = playHand (drop (boxCardCount left - 1) deck) (getSplitCount left) dealerHand (PlayerDeck (Hand [head (tail (getCards (getPlayerHand playerDeck)))] True False) (getBet playerDeck))
        Box (getPlayerDecks left ++ getPlayerDecks right) (getSplitCount right)
    | canDouble (getPlayerHand playerDeck) && voteDouble (getPlayerHand playerDeck) = 
        Box [PlayerDeck (Hand (getCards (getPlayerHand playerDeck) ++ [head deck]) (isSplitted playerDeck) True) (2.0 * getBet playerDeck)] splitCount
    | canHit (getPlayerHand playerDeck) && voteHit dealerHand (getPlayerHand playerDeck) = 
        foldl 
            (\x y -> Box (getPlayerDecks x ++ getPlayerDecks y) (getSplitCount y))
            (Box [] 0) 
            (map (playHand (tail deck) splitCount dealerHand) [PlayerDeck (Hand (getCards (getPlayerHand playerDeck) ++ [head deck]) (isSplitted playerDeck) False) (getBet playerDeck)])
    | otherwise = Box [playerDeck] splitCount

-- |Returns 'True' if the two cards have the same value
canSplit :: Hand -> Int -> Bool
canSplit hand splitCount = splitCount < 3 && length (getCards hand) == 2 && handValue (take 1 (getCards hand)) == handValue (drop 1 (getCards hand))

voteSplit :: Hand -> Hand -> Bool
voteSplit dealerHand playerHand = True

-- |Returns 'True' if the value of the hand is less than 21 and the hand is not doubled
canHit :: Hand -> Bool
canHit hand = handValue (getCards hand) < 21 && not (isDoubled hand)

voteHit :: Hand -> Hand -> Bool
voteHit dealerHand playerHand = handValue (getCards playerHand) < 17

-- |Returns 'True' if the two cards of the hand sum up to 9, 10 or 11
-- TODO in a doubled hand with soft value card (ace) the ace always counts 11
-- TODO an a splitted ace only one card is dealed (but can split up to 3 times if ace is dealed)
canDouble :: Hand -> Bool
canDouble hand = (length (getCards hand) == 2) && (handValue (getCards hand) == 9 || handValue (getCards hand) == 10 || handValue (getCards hand) == 11)

voteDouble :: Hand -> Bool
voteDouble hand = True

evaluateEarnings :: Table -> Float
evaluateEarnings table =
    winHandSum table
    + blackJackHandSum table
    - looseHandSum table
    - bustHandSum table

isDealerBust :: Table -> Bool
isDealerBust table = isBust (getCards (getDealerHand table))

isDealerNotBust :: Table -> Bool
isDealerNotBust table = not (isDealerBust table)

bustHandSum :: Table -> Float
bustHandSum table = sum (map getBet
    (filter (isBust . getCards . getPlayerHand) (getPlayerDecks (getPlayerBox table))))

looseHandSum :: Table -> Float
looseHandSum table = sum (map getBet
    (filter (\d -> (isNotBlackJack (getPlayerHand d)
    && isNotBlackJack (getDealerHand table)
    && isNotBust (getCards (getPlayerHand d))
    && isDealerNotBust table 
    && (handValue (getCards (getPlayerHand d)) < handValue (getCards (getDealerHand table))))
    || (isNotBlackJack (getPlayerHand d) 
        && isBlackJack (getDealerHand table))) (getPlayerDecks (getPlayerBox table))))

winHandSum :: Table -> Float
winHandSum table = sum (map getBet 
    (filter (\d -> isNotBlackJack (getPlayerHand d)
    && isNotBlackJack (getDealerHand table)
    && isNotBust (getCards (getPlayerHand d))
    && isDealerNotBust table 
    && (handValue (getCards (getPlayerHand d)) > handValue (getCards (getDealerHand table)))) (getPlayerDecks (getPlayerBox table))))

blackJackHandSum :: Table -> Float
blackJackHandSum table = sum (map (\h -> 3.0 / 2.0 * getBet h)
    (filter (\d -> isBlackJack (getPlayerHand d) 
    && isNotBlackJack (getDealerHand table)) (getPlayerDecks (getPlayerBox table))))
