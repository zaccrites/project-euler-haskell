
import Data.Maybe (fromJust, fromMaybe)
import Data.List (maximumBy, find, group, sort)

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Enum, Ord)

readRawValue :: Char -> Value
readRawValue '2' = Two
readRawValue '3' = Three
readRawValue '4' = Four
readRawValue '5' = Five
readRawValue '6' = Six
readRawValue '7' = Seven
readRawValue '8' = Eight
readRawValue '9' = Nine
readRawValue 'T' = Ten
readRawValue 'J' = Jack
readRawValue 'Q' = Queen
readRawValue 'K' = King
readRawValue 'A' = Ace

readRawSuit :: Char -> Suit
readRawSuit 'H' = Hearts
readRawSuit 'D' = Diamonds
readRawSuit 'C' = Clubs
readRawSuit 'S' = Spades


data Card = Card Value Suit
type Hand = [Card]

data HandType =
    HighCard Value |  -- high card
    OnePair Value Value |  -- pair, high card
    TwoPairs Value Value Value |  -- high pair, low pair, high card
    ThreeOfAKind Value Value |  -- triplet, high card
    Straight Value |  -- high card
    Flush Value |  -- high card
    FullHouse Value Value |  -- triplet, pair
    FourOfAKind Value Value |  -- quadruplet, high card
    StraightFlush Value  -- high card
    -- A royal flush is just the best possible straight flush.
    deriving (Eq, Ord)


getStraightFlush :: Hand -> Maybe HandType
getStraightFlush cards = case (getStraight cards, getFlush cards) of
    (Just (Straight x), Just (Flush _)) -> Just (StraightFlush x)
    _ -> Nothing


getFlush :: Hand -> Maybe HandType
getFlush cards
    | all (==suit) suits  = Just (Flush (maximum values))
    | otherwise = Nothing
    where (suit:suits) = map (\(Card _ x) -> x) cards
          values = map (\(Card x _) -> x) cards


getStraight :: Hand -> Maybe HandType
getStraight cards
    | values == [head values .. last values] = Just (Straight (maximum values))
    | otherwise = Nothing
    where values = sort $ map (\(Card x _) -> x) cards


getFullHouse :: Hand -> Maybe HandType
getFullHouse cards = case (triplet, pair) of
    (Just x, Just y) -> Just (FullHouse x y)
    _ -> Nothing
    where values = sort $ map (\(Card x _) -> x) cards
          triplets = map head $ filter ((==3) . length) $ group values
          pairs = map head $ filter ((==2) . length) $ group values
          triplet = if null triplets then Nothing else Just (maximum triplets)
          pair = if null pairs then Nothing else Just (maximum pairs)


getNOfAKind :: Int -> Hand -> Maybe HandType
getNOfAKind n cards = case (n, multiple) of
    (2, Just x) -> Just (OnePair x highCard)
    (3, Just x) -> Just (ThreeOfAKind x highCard)
    (4, Just x) -> Just (FourOfAKind x highCard)
    _ -> Nothing
    where values = sort $ map (\(Card x _) -> x) cards
          multiple = fmap head $ find ((==n) . length) $ group values
          highCard = maximum $ filter ((/= fromJust multiple)) $ values


getTwoPairs :: Hand -> Maybe HandType
getTwoPairs cards
    | length pairs == 2  = Just (TwoPairs (maximum pairs) (minimum pairs) highCard)
    | otherwise = Nothing
    where values = sort $ map (\(Card x _) -> x) cards
          pairs = map head $ filter ((==2) . length) $ group values
          highCard = maximum $ filter (`notElem` pairs) $ values


getHandType :: Hand -> HandType
getHandType cards = fromMaybe highCard (maximum possibleHandTypes) where
    possibleHandTypes = map ($ cards) [getStraightFlush, getNOfAKind 4, getFullHouse, getFlush, getStraight, getNOfAKind 3, getTwoPairs, getNOfAKind 2]
    highCard = HighCard (maximum $ map (\(Card x _) -> x) cards)


split :: (a -> Bool) -> [a] -> [[a]]
split p xs = case dropWhile p xs of
    []  -> []
    xs' -> part : split p xs'' where
        (part, xs'') = break p xs'

readHands :: String -> [(Hand, Hand)]
readHands contents = hands where
    hands = map (splitAt 5 . map readCard . split (==' ')) $ lines contents
    readCard [rawValue, rawSuit] = Card (readRawValue rawValue) (readRawSuit rawSuit)


problem54 contents = length $ filter id player1Wins where
    player1Wins = map (\(x, y) -> x > y) handTypes
    handTypes = map (\(x, y) -> (getHandType x, getHandType y)) hands
    hands = readHands contents

main = do
    contents <- readFile "resources/p054_poker.txt"
    print $ problem54 contents
