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
data Player = Player Name Deck Stack Stack [Stack] deriving (Show)

playerName :: Player -> Name
playerDeck :: Player -> Deck
playerNertz :: Player -> Stack
playerHand :: Player -> Stack
playerSoli :: Player -> [Stack]
updateDeck :: Player -> Deck -> Player
createPlayer :: Name -> Player

playerName (Player name _ _ _ _) = name
playerDeck (Player _ deck _ _ _) = deck
playerNertz (Player _ _ nertzStack _ _) = nertzStack
playerHand (Player _ _ _ handStack _) = handStack
playerSoli (Player _ _ _ _ soliStack) = soliStack

updateDeck player newDeck = Player (playerName player) newDeck (playerNertz player) (playerHand player) (playerSoli player)


createPlayer name = Player name (map (setCardName name) (buildDeck 51)) [] [] [] where

    setCardName name card = Card (cardSuit card) (cardValue card) name
    buildDeck 0 = [Card 0 1 ""]
    buildDeck d = buildDeck (d-1) ++ [Card (div d 13) (mod d 13 + 1) ""]


initNertz :: Player -> Player  
initNertz p = p

playerNames = ["Alf", "Bob", "Cat", "Dog"]



players = map createPlayer playerNames





