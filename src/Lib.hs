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
cardVal :: Card -> Value
cardName :: Card -> Name

cardSuit (Card suit value name) = suit
cardVal (Card suit value name) = value
cardName (Card suit value name) = name

oppositeSuits :: Card -> Card -> Bool
oppositeSuits card1 card2 = mod (cardSuit card1) 2 /= mod (cardSuit card2) 2

canStackSoli :: Card -> Card -> Bool
canStackSoli card1 card2 = oppositeSuits card1 card2 && cardVal card1 == cardVal card2 - 1

canStackMid :: Card -> Card -> Bool
canStackMid card1 card2 = cardSuit card1 == cardSuit card2 && cardVal card1 == cardVal card2 + 1

-- Stack methods
takeTopCard :: Stack -> Card
takeTopCard = head

removeTopCard :: Stack -> Stack
removeTopCard = tail

-- Player Object & methods
data PlayerInfo = PlayerInfo Name Int Int deriving (Show)
data PlayerCards = PlayerCards Deck Stack Stack Stack Stack Stack Stack deriving (Show)
data Player = Player PlayerInfo PlayerCards Action deriving (Show)

data Action =   Idle |
                Wait |
                DrawThree |
                AceToMid |
                NertzToMid Stack |
                NertzToSoli Int |
                HandToMid Stack |
                HandToSoli Int |
                SoliToSoli Int Int |
                SoliToMid Int Stack deriving (Show)

--data PlayerAction = PlayerAction Action


data Table = Table [Player] [Stack] deriving (Show)

tablePlayers :: Table -> [Player]
tableStacks :: Table -> [Stack]

tablePlayers (Table players _) = players
tableStacks (Table _ stacks) = stacks

playerCards :: Player -> PlayerCards
playerInfo :: Player -> PlayerInfo
playerAction :: Player -> Action

playerName :: PlayerInfo -> Name
playerDeck :: PlayerCards -> Deck
playerCutVal :: PlayerInfo -> Int
playerScore :: PlayerInfo -> Int
playerNertz :: PlayerCards -> Stack
playerHand :: PlayerCards -> Stack
playerSoli1 :: PlayerCards -> Stack
playerSoli2 :: PlayerCards -> Stack
playerSoli3 :: PlayerCards -> Stack
playerSoli4 :: PlayerCards -> Stack
--updateDeck :: Player -> Deck -> Player
createPlayer :: (Name, Int) -> Player

playerName   (PlayerInfo name _ _) = name
playerCutVal (PlayerInfo _ cutVal _) = cutVal
playerScore (PlayerInfo _ _ score) = score
playerDeck  (PlayerCards deck _ _ _ _ _ _) = deck
playerNertz (PlayerCards _ nertzStack _ _ _ _ _) = nertzStack
playerHand  (PlayerCards _ _ handStack _ _ _ _) = handStack
playerSoli1 (PlayerCards _ _ _ soli _ _ _) = soli
playerSoli2 (PlayerCards _ _ _ _ soli _ _) = soli
playerSoli3 (PlayerCards _ _ _ _ _ soli _) = soli
playerSoli4 (PlayerCards _ _ _ _ _ _ soli) = soli

playerInfo (Player info _ _) = info
playerCards (Player _ cards _) = cards
playerAction (Player _ _ action) = action

--updateDeck player newDeck = Player (playerName player) (playerCutVal player) newDeck (playerNertz player) (playerHand player) (playerSoli player)


createPlayer (name, cutVal) = Player pinfo pcards pact where --PlayerInfo(name cutVal) PlayerCards(map (setCardName name) (buildDeck 51) [] [] [] [] [] []) where
    score = 0
    pinfo = PlayerInfo name cutVal score
    pcards = PlayerCards (map (setCardName name) (buildDeck 51)) [] [] [] [] [] []
    setCardName name card = Card (cardSuit card) (cardVal card) name
    buildDeck 0 = [Card 0 1 ""]
    buildDeck d = buildDeck (d-1) ++ [Card (div d 13) (mod d 13 + 1) ""]
    pact = Wait    
--Player name cutVal (map (setCardName name) (buildDeck 51)) [] [] [] where


playerTopCards :: PlayerCards -> [Card]
playerTopCards player = map head $ [playerNertz player] ++ [playerHand player] ++ [playerSoli1 player] ++ [playerSoli2 player] ++ [playerSoli3 player] ++ [playerSoli4 player]


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


-- Take a subset of cards from a stack of Cards. Start and End are inclusive    
takeCards :: (Int, Int) -> [Card] -> [Card]
takeCards (start, end) cards = take (end-start+1) $ drop start cards


setupPlayerStacks :: Player -> Player
setupPlayerStacks player = player' where
    pinfo = playerInfo player
    pcards = playerCards player
    pact = playerAction player
    deck' = shuffleNTimes 233 (playerCutVal pinfo) (playerDeck pcards)
    nertz' = takeCards (0,12) deck'
    soli1' = takeCards (13,13) deck'
    soli2' = takeCards (14,14) deck'
    soli3' = takeCards (15,15) deck'
    soli4' = takeCards (16,16) deck'
    hand' = takeCards (17,51) deck'
    pcards' = PlayerCards deck' nertz' hand' soli1' soli2' soli3' soli4'
    player' = Player pinfo pcards' pact



getPlayerActions :: Table -> Table
getPlayerActions table = table' where
    players = tablePlayers table
    midStacks = tableStacks table
    players' = foldr (\ player -> (++) [getPlayerAction player midStacks]) [] players
    table' = Table players' midStacks

getPlayerAction :: Player -> [Stack] -> Player
getPlayerAction player midStacks = player' where
    pinfo = playerInfo player
    pcards = playerCards player

    topNertz = head $ playerNertz pcards
    topSolis = map head $ [playerSoli1 pcards] ++ [playerSoli2 pcards] ++ [playerSoli3 pcards] ++ [playerSoli4 pcards]
    topHand = head $ playerHand pcards
    topMids = map head midStacks
    topCards = [topNertz] ++ [topHand] ++ topSolis

    canAceToMid = foldr (((||) . (==1)) . cardVal) False topCards
    canNertzToMid = foldr (||) False $ map (canStackMid topNertz) topMids
    canNertzToSoli = foldr (||) False $ map (canStackSoli topNertz) topSolis
    canHandToMid = foldr (||) False $ map (canStackMid topHand) topMids
    canSoliToMid = foldr (||) False $ getPerms topSolis topMids canStackMid
    --canSoliToSoli


    pact' :: Action
    pact'
        | canAceToMid = AceToMid
        | canNertzToMid = NertzToMid []
        | canNertzToSoli = NertzToSoli 0
        | canHandToMid = HandToMid []
        | canSoliToMid = SoliToMid 0 []
        | otherwise = DrawThree

    player' = Player pinfo pcards pact'


{-
    pcards = playerCards player
    action = playerAction player
    nertz = playerNertz pcards
    hand = playerHand pcards
    soli1 = playerSoli1 pcards
    soli2 = playerSoli2 pcards
    soli3 = playerSoli3 pcards
    soli4 = playerSoli4 pcards
    action' = action
-}
--canNertzToMid :: Stack -> Bool

canStackOnMidStacks :: [Stack] -> Card -> Bool
canStackOnMidStacks midStacks card = foldr (&&) True $ map (canStackOnMid card) midStacks


canStackOnMid :: Card -> Stack -> Bool
canStackOnMid card stack = canStack where
    canStack = isAce || cardSuit card == cardSuit topCard && cardVal card == (cardVal topCard + 1)
    isAce = cardVal card == 1
    topCard = head stack


playerNames = ["Alf", "Bob", "Cat", "Dog"]
playerCutVals = [10, 20, 30, 40]
playerInfos = zip playerNames playerCutVals


players = map (setupPlayerStacks . createPlayer) playerInfos

table = Table players []
table' = getPlayerActions table
players' = tablePlayers table'

jonah = (setupPlayerStacks . createPlayer) ("jonah", 17)
jd = playerDeck $ playerCards jonah 
jn = playerCutVal $ playerInfo jonah

doSomething :: [Card] -> [Int]
doSomething (card:cards) = [cardVal card] ++ doSomething cards
doSomething [] = []


cardVals :: [[Card]] -> [[Value]]
cardVals (c:cs) = [map cardVal c] ++ cardVals cs
cardVals [] = []


bill = createPlayer ("Bill",1)
bd = playerDeck $ playerCards bill

oneHeart = bd !! 0
twoHeart = bd !! 1
threeHeart = bd !! 2


addEach i j = i + j

t1 = [1,2,3]
t2 = [4,5,6]


getPerms xs ys f = foldr (\ x -> (++) (map (f x) ys)) [] xs