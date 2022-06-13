module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- Type declarations
type Name = String
type Value = Int
type Suit = Int
type Deck = [Card]
type Stack = [Card]

-- Card Object & methods
data Card = Card Suit Value Name deriving (Show)

cardSuit :: Card -> Suit
cardValue :: Card -> Value
cardName :: Card -> Name

cardSuit (Card suit value name) = suit
cardValue (Card suit value name) = value
cardName (Card suit value name) = name


-- Stack methods
takeTopCard :: Stack -> Card
takeTopCard = head

removeTopCard :: Stack -> Stack
removeTopCard = tail

-- Player Object & methods
data PlayerInfo = PlayerInfo Name Int deriving (Show)
data PlayerCards = PlayerCards Deck Stack Stack Stack Stack Stack Stack deriving (Show)
data Player = Player PlayerInfo PlayerCards deriving (Show)

playerCards :: Player -> PlayerCards
playerInfo :: Player -> PlayerInfo

playerName :: PlayerInfo -> Name
playerDeck :: PlayerCards -> Deck
playerCutVal :: PlayerInfo -> Int
playerNertz :: PlayerCards -> Stack
playerHand :: PlayerCards -> Stack
--playerSoli :: PlayerCards -> [Stack]
--updateDeck :: Player -> Deck -> Player
createPlayer :: (Name, Int) -> Player

playerName (PlayerInfo name _ ) = name
playerCutVal (PlayerInfo _ cutVal) = cutVal
playerDeck (PlayerCards deck _ _ _ _ _ _) = deck
playerNertz (PlayerCards _ nertzStack _ _ _ _ _) = nertzStack
playerHand (PlayerCards _ _ handStack _ _ _ _) = handStack
--playerSoli (PlayerCards _ _ _ soliStack) = soliStack

playerInfo (Player info _) = info
playerCards (Player _ cards) = cards

--updateDeck player newDeck = Player (playerName player) (playerCutVal player) newDeck (playerNertz player) (playerHand player) (playerSoli player)


createPlayer (name, cutVal) = Player pinfo pcards where --PlayerInfo(name cutVal) PlayerCards(map (setCardName name) (buildDeck 51) [] [] [] [] [] []) where
    pinfo = PlayerInfo name cutVal
    pcards = PlayerCards (map (setCardName name) (buildDeck 51)) [] [] [] [] [] []
    setCardName name card = Card (cardSuit card) (cardValue card) name
    buildDeck 0 = [Card 0 1 ""]
    buildDeck d = buildDeck (d-1) ++ [Card (div d 13) (mod d 13 + 1) ""]    
--Player name cutVal (map (setCardName name) (buildDeck 51)) [] [] [] where


transferCard :: (Stack, Stack) -> (Stack, Stack)
transferCard (stackFrom, stackTo) = (tail stackFrom, head stackFrom:stackTo)

shuffleNTimes :: Int -> Int -> [Card] -> [Card]
shuffleNTimes 1 cutVal deck = shuffle cutVal deck
shuffleNTimes n cutVal deck = shuffleNTimes (n-1) cutVal shuffledDeck where
    shuffledDeck = shuffle cutVal deck


shuffle :: Int -> [Card] -> [Card]
shuffle cutVal deck = shuffledDeck where
    halfLen = div (length deck) 2 -- half-way index of deck
    sd1 = take halfLen deck -- 1st half of deck
    sd2 = drop halfLen deck -- 2nd half of deck
    almostShuffledDeck = concat (zipWith (\a b -> [a, b]) sd1 sd2) -- interleave 2 halves
    shuffledDeck = drop cutVal almostShuffledDeck ++ take cutVal almostShuffledDeck -- cut the deck at 'cutVal' index 


playerNames = ["Alf", "Bob", "Cat", "Dog"]
playerCutVals = [10, 20, 30, 40]
playerInfos = zip playerNames playerCutVals

players = map createPlayer playerInfos


jonah = createPlayer ("jonah", 17)
jd = playerDeck $ playerCards jonah 
jn = playerCutVal $ playerInfo jonah

