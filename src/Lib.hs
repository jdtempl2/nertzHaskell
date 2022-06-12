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
data PlayerDeck = PlayerDeck Name Deck
data Player = Player Name Int Deck Stack Stack [Stack] deriving (Show)

playerName :: Player -> Name
playerDeck :: Player -> Deck
playerCutVal :: Player -> Int
playerNertz :: Player -> Stack
playerHand :: Player -> Stack
playerSoli :: Player -> [Stack]
updateDeck :: Player -> Deck -> Player
createPlayer :: Name -> Int -> Player

playerName (Player name _ _ _ _ _) = name
playerCutVal (Player _ cutVal _ _ _ _) = cutVal
playerDeck (Player _ _ deck _ _ _) = deck
playerNertz (Player _ _ _ nertzStack _ _) = nertzStack
playerHand (Player _ _ _ _ handStack _) = handStack
playerSoli (Player _ _ _ _ _ soliStack) = soliStack

updateDeck player newDeck = Player (playerName player) (playerCutVal player) newDeck (playerNertz player) (playerHand player) (playerSoli player)


createPlayer name cutVal = Player name cutVal (map (setCardName name) (buildDeck 51)) [] [] [] where

    setCardName name card = Card (cardSuit card) (cardValue card) name
    buildDeck 0 = [Card 0 1 ""]
    buildDeck d = buildDeck (d-1) ++ [Card (div d 13) (mod d 13 + 1) ""]

transferCard :: (Stack, Stack) -> (Stack, Stack)
transferCard (stackFrom, stackTo) = (tail stackFrom, head stackFrom:stackTo)

shuffleNTimes :: Int -> Int -> [Card] -> [Card]
shuffleNTimes 1 cutVal deck = shuffle cutVal deck
shuffleNTimes n cutVal deck = shuffleNTimes (n-1) cutVal shuffledDeck where
    shuffledDeck = shuffle cutVal deck


shuffle :: Int -> [Card] -> [Card]
shuffle cutVal deck = shuffledDeck where
    halfLen = div (length deck) 2
    sd1 = take halfLen deck
    sd2 = drop halfLen deck
    turnToList a b = [a,b]
    almostShuffledDeck = concat (zipWith (\a b -> [a, b]) sd1 sd2) 
    shuffledDeck = drop cutVal almostShuffledDeck ++ take cutVal almostShuffledDeck


playerNames = ["Alf", "Bob", "Cat", "Dog"]
playerCutVals = [10, 20, 30, 40]


players = map createPlayer playerNames

jonah = createPlayer "jonah" 17
jd = playerDeck jonah 
jn = playerCutVal jonah

