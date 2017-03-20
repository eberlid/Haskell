module BlackJack (play, getCards, freshHand, Table, Deck, Hand(..), dealerAction, canSplit, canDouble, handValue, isBlackJack, createDeck, showTables, Card(..), CardValue (Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace), Suit (Spade, Diamond, Club, Heart)) where

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
    deriving (Show, Eq)

data PlayerDeck = PlayerDeck Hand Float
    deriving (Show)

-- | Declare 'Box' as a synonym of array of 'Deck'
data Box = Box [PlayerDeck] Int
    deriving (Show)

data Table = Table Deck Hand Box Int
    deriving (Show)

freshHand :: [Card] -> Hand
freshHand cards = Hand cards False False

shortCardValue :: CardValue -> String
shortCardValue value 
    | value == Two = "2"
    | value == Three = "3"
    | value == Four = "4"
    | value == Five = "5"
    | value == Six = "6"
    | value == Seven = "7"
    | value == Eight = "8"
    | value == Nine = "9"
    | value == Ten = "10"
    | value == Jack = "J"
    | value == Queen = "Q"
    | value == King = "K"
    | value == Ace = "A"
    | otherwise = "x"

shortCardSuit :: Suit -> String
shortCardSuit suit 
    | suit == Spade = "S"
    | suit == Club = "C"
    | suit == Diamond = "D"
    | suit == Heart = "H"
    | otherwise = "x"

-- |Returns the specified number of 52-card decks as on ordered list
createDeck :: Int -> Deck
createDeck deckCount =
    if deckCount <= 0 || deckCount > 6
    then []
    else [Card val su | val <- [Two .. Ace], su <- [Club .. Spade]] ++ createDeck (deckCount - 1)

shortCard :: Card -> String
shortCard card = shortCardValue (getCardValue card) ++ shortCardSuit (getCardSuit card)

showTables :: [Table] -> String
showTables tables = concatMap showTable tables ++
    "    > Balance: " ++
        show (foldl (\ x y -> x + evaluateEarnings y) 0 tables)

showTable :: Table -> String
showTable table = "Playing loop " ++ show (getLoopNumber table) ++ "\r\n" ++ showDealerDeck table ++ "\r\n" ++ showPlayerBox table ++ "\r\n" ++ "    > Preview: " ++ showDeckPreview table ++ "\r\n"

showDeckPreview :: Table -> String
showDeckPreview table = show (take 15 (map shortCard (getDeck table)))

showDealerDeck :: Table -> String
showDealerDeck table = "    > Dealer Deck (" ++ (if isBlackJack (getDealerHand table) then "BJ" else show (handValue (getDealerHand table))) ++ "): " ++ show (getCards (getDealerHand table))

showPlayerBox :: Table -> String
showPlayerBox table = "    > Box SplitCount: " ++ show (getSplitCount (getPlayerBox table)) ++ "\r\n" ++
    concatMap showPlayerDeck (getPlayerDecks (getPlayerBox table)) 
        ++ "        > Yield: " ++ (if evaluateEarnings table > 0 then "+" else "") ++ show (evaluateEarnings table)

showPlayerDeck :: PlayerDeck -> String
showPlayerDeck playerDeck = "        > Player Deck [" ++ showHandMode (getPlayerHand playerDeck) ++ "] (" ++
        (if isBlackJack (getPlayerHand playerDeck) then "BJ" else show (handValue (getPlayerHand playerDeck)))
            ++ "): " ++ show (getCards (getPlayerHand playerDeck)) ++ "\r\n"

showHandMode :: Hand -> String
showHandMode hand = (if isSplittedHand hand then "S" else "-") ++ (if isDoubled hand then "D" else "-")


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

getCardCount :: Table -> Int
getCardCount table = length (getCards (getDealerHand table)) + foldl (\x y -> x + length (getCards (getPlayerHand y))) 0 (getPlayerDecks (getPlayerBox table))

getLoopNumber :: Table -> Int
getLoopNumber (Table _ _ _ l) = l

-- |Returns the Deck of a snapshot
getDeck :: Table -> Deck
getDeck (Table deck _ _ _) = deck

-- |Returns the Dealer Hand of the table
getDealerHand :: Table -> Hand
getDealerHand (Table _ dealerHand _ _) = dealerHand

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
getPlayerBox (Table _ _ playerBox _) = playerBox

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
isBust :: Hand -> Bool
isBust hand = handValue hand > 21

isNotBust :: Hand -> Bool
isNotBust hand = not (isBust hand)

handValue :: Hand -> Int
handValue hand = if isDoubled hand then sumCardValues (getCards hand) 0 else sumCardValues (getCards hand) (acesAsOneCount (getCards hand))

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

bet :: Float
bet = 1.0

play :: Deck -> Int -> [Table]
play deck numberOfRounds = do 
    playLoop deck numberOfRounds 0 0

playLoop :: Deck -> Int -> Int -> Float -> [Table]
playLoop deck numberOfRounds roundsPlayed balance = do
    let table = (playBox (drop 3 deck) (Hand [deck !! 1] False False) (PlayerDeck (Hand [head deck, deck !! 2] False False) bet)) roundsPlayed
    let earnings = evaluateEarnings table
    if roundsPlayed < numberOfRounds 
        then table : playLoop (getDeck table) numberOfRounds (roundsPlayed + 1) (balance + earnings)
        else [table]

playBox :: Deck -> Hand -> PlayerDeck -> Int -> Table
playBox deck dealerHand playerDeck loopNumber = do
    let playerBox = playHand deck 0 dealerHand playerDeck
    let playerBoxDealedCardCount = boxCardCount playerBox - 2

    let finalDealerHand = dealerAction (drop playerBoxDealedCardCount deck) dealerHand
    let dealerHandDealedCardCount = length (getCards finalDealerHand) - 1
    
    Table (drop (playerBoxDealedCardCount + dealerHandDealedCardCount) deck) finalDealerHand playerBox loopNumber

dealerAction :: Deck -> Hand -> Hand
dealerAction deck dealerHand 
    | handValue dealerHand < 17 = dealerAction (tail deck) (Hand (getCards dealerHand ++ [head deck]) False False)
    | otherwise = dealerHand

playHand :: Deck -> Int -> Hand -> PlayerDeck -> Box
playHand deck boxSplitCount dealerHand playerDeck
    | canSplit (getPlayerHand playerDeck) boxSplitCount && voteSplit dealerHand (getPlayerHand playerDeck) = do
        let leftCard = head (getCards (getPlayerHand playerDeck))
        let rightCard = head (tail (getCards (getPlayerHand playerDeck)))
        let left = if isAce leftCard
            then Box [PlayerDeck (Hand (getCards (getPlayerHand playerDeck) ++ [head deck]) True False) (getBet playerDeck)] (boxSplitCount + 1)
            else playHand deck (boxSplitCount + 1) dealerHand (PlayerDeck (Hand [leftCard] True False) (getBet playerDeck))
        let right = if isAce rightCard 
            then Box [PlayerDeck (Hand (getCards (getPlayerHand playerDeck) ++ [head (drop (boxCardCount left - 1) deck)]) True False) (getBet playerDeck)] (boxSplitCount + 1)
            else playHand (drop (boxCardCount left - 1) deck) (getSplitCount left) dealerHand (PlayerDeck (Hand [rightCard] True False) (getBet playerDeck))
        Box (getPlayerDecks left ++ getPlayerDecks right) (getSplitCount right)
    | canDouble (getPlayerHand playerDeck) && voteDouble (getPlayerHand playerDeck) = 
        Box [PlayerDeck (Hand (getCards (getPlayerHand playerDeck) ++ [head deck]) (isSplitted playerDeck) True) (2.0 * getBet playerDeck)] boxSplitCount
    | canHit (getPlayerHand playerDeck) && voteHit dealerHand (getPlayerHand playerDeck) = 
        foldl (\x y -> Box (getPlayerDecks x ++ getPlayerDecks y) (getSplitCount y)) (Box [] 0) 
            (map (playHand (tail deck) boxSplitCount dealerHand) [PlayerDeck (Hand (getCards (getPlayerHand playerDeck) ++ [head deck]) (isSplitted playerDeck) False) (getBet playerDeck)])
    | otherwise = Box [playerDeck] boxSplitCount

-- |Returns 'True' if the two cards have the same value
canSplit :: Hand -> Int -> Bool
canSplit hand boxSplitCount = 
    boxSplitCount < 3 
    && length (getCards hand) == 2 
    && handValue (Hand (take 1 (getCards hand)) (isSplittedHand hand) (isDoubled hand)) 
        == handValue (Hand (drop 1 (getCards hand)) (isSplittedHand hand) (isDoubled hand))

voteSplit :: Hand -> Hand -> Bool
voteSplit dealerHand playerHand = True

-- |Returns 'True' if the value of the hand is less than 21 and the hand is not doubled
canHit :: Hand -> Bool
canHit hand = handValue hand < 21 && not (isDoubled hand)

voteHit :: Hand -> Hand -> Bool
voteHit dealerHand playerHand = handValue playerHand < 17

-- |Returns 'True' if the two cards of the hand sum up to 9, 10 or 11
canDouble :: Hand -> Bool
canDouble hand = length (getCards hand) == 2 -- && (handValue hand == 9 || handValue hand == 10 || handValue hand == 11)

voteDouble :: Hand -> Bool
voteDouble hand = True

evaluateEarnings :: Table -> Float
evaluateEarnings table =
    winHandSum table
    + blackJackHandSum table
    - looseHandSum table
    - bustHandSum table

isDealerBust :: Table -> Bool
isDealerBust table = isBust (getDealerHand table)

isDealerNotBust :: Table -> Bool
isDealerNotBust table = not (isDealerBust table)

bustHandSum :: Table -> Float
bustHandSum table = sum (map getBet
    (filter (isBust . getPlayerHand) (getPlayerDecks (getPlayerBox table))))

looseHandSum :: Table -> Float
looseHandSum table = sum (map getBet
    (filter (\d -> (isNotBlackJack (getPlayerHand d)
    && isNotBlackJack (getDealerHand table)
    && isNotBust (getPlayerHand d)
    && isDealerNotBust table 
    && (handValue (getPlayerHand d) < handValue (getDealerHand table)))
    || (isNotBlackJack (getPlayerHand d) 
        && isBlackJack (getDealerHand table))) (getPlayerDecks (getPlayerBox table))))

winHandSum :: Table -> Float
winHandSum table = sum (map getBet 
    (filter (\d -> isNotBlackJack (getPlayerHand d)
    && isNotBlackJack (getDealerHand table)
    && isNotBust (getPlayerHand d)
    && isDealerNotBust table 
    && (handValue (getPlayerHand d) > handValue (getDealerHand table))) (getPlayerDecks (getPlayerBox table))))

blackJackHandSum :: Table -> Float
blackJackHandSum table = sum (map (\h -> 3.0 / 2.0 * getBet h)
    (filter (\d -> isBlackJack (getPlayerHand d) 
    && isNotBlackJack (getDealerHand table)) (getPlayerDecks (getPlayerBox table))))
