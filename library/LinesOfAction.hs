{-# LANGUAGE TupleSections #-}
module LinesOfAction where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Safe (headMay)
import Data.Maybe (catMaybes)

data Checker
    = Checker'Black
    | Checker'White
    deriving (Show, Eq)

newtype Board = Board (Map.Map (Int, Int) Checker)
    deriving (Eq)

instance Show Board where
    show (Board m) = "\n  01234567\n" ++ concat [ show y ++ " " ++ [showChecker $ Map.lookup (x,y) m | x <- [0..7] ] ++ "\n" | y <- [0..7]]
        where
            showChecker :: Maybe Checker -> Char
            showChecker Nothing = '·'
            showChecker (Just c) = case c of
                Checker'White -> 'W'
                Checker'Black -> 'B'

data Terminal
    = Terminal'Winner Checker
    | Terminal'Draw
    deriving (Show, Eq)

data Move = Move
    { moveFrom :: (Int, Int)
    , moveTo :: (Int, Int)
    , moveChecker :: Checker
    } deriving (Show, Eq)

class Monad m => Game m where
    getMove :: Board -> Checker -> m Move
    invalidateMove :: Board -> Checker -> Move -> m ()
    validatedMove :: Board -> Board -> Checker -> Move -> m ()
    terminate :: Board -> Terminal -> m ()
    applyMove :: Board -> Move -> m (Maybe Board)
    applyMove board move = return $ applyMove' board move

startGame :: Game m => m ()
startGame = play emptyBoard Checker'Black

play :: Game m => Board -> Checker -> m ()
play b c = case terminal b of
    Nothing -> do
        move <- getMove b c
        applied <- applyMove b move
        case applied of
            Nothing -> invalidateMove b c move
            Just b' -> do
                validatedMove b b' c move
                play b' (opponent c)
    Just term -> terminate b term

applyMove' :: Board -> Move -> Maybe Board
applyMove' board@(Board m) move@Move{moveFrom,moveTo,moveChecker} = do
    checker <- Map.lookup moveFrom m
    cells <- movableCells board moveFrom
    if elem moveTo cells && checker == moveChecker
        then Just $ placeCheckerOnBoard board move
        else Nothing

placeCheckerOnBoard :: Board -> Move -> Board
placeCheckerOnBoard (Board m) move = Board
    $ Map.delete (moveFrom move)
    $ Map.insert (moveTo move) (moveChecker move) m

movableCells :: Board -> (Int, Int) -> Maybe [(Int, Int)]
movableCells b@(Board m) xy = flip (movableCellsWithChecker b) xy <$> Map.lookup xy m

movableCellsWithChecker :: Board -> Checker -> (Int, Int) -> [(Int,Int)]
movableCellsWithChecker b checker xy = catMaybes
    [ filterOrdered checker (lookupCells b $ listIndicesVertUp vertCount xy)
    , filterOrdered checker (lookupCells b $ listIndicesVertDown vertCount xy)
    , filterOrdered checker (lookupCells b $ listIndicesHorzLeft horzCount xy)
    , filterOrdered checker (lookupCells b $ listIndicesHorzRight horzCount xy)
    , filterOrdered checker (lookupCells b $ listIndicesUpwardsLeft upCount xy) 
    , filterOrdered checker (lookupCells b $ listIndicesUpwardsRight upCount xy) 
    , filterOrdered checker (lookupCells b $ listIndicesDownwardsLeft downCount xy)
    , filterOrdered checker (lookupCells b $ listIndicesDownwardsRight downCount xy)
    ]
  where
    vertCount = verticalCount b xy
    horzCount = horizontalCount b xy
    upCount = upwardsCount b xy
    downCount = downwardsCount b xy

lookupCells :: Board -> [(Int, Int)] -> [((Int, Int), Maybe Checker)]
lookupCells (Board m) xs = zip xs (map (flip Map.lookup m) xs)

listIndicesVertUp, listIndicesVertDown, listIndicesHorzLeft, listIndicesHorzRight, listIndicesUpwardsLeft, listIndicesUpwardsRight, listIndicesDownwardsLeft, listIndicesDownwardsRight :: Int -> (Int, Int) -> [(Int, Int)]
listIndicesVertUp count xy = listIndices (\y -> (0,-y)) count xy
listIndicesVertDown count xy = listIndices (\y -> (0,y)) count xy
listIndicesHorzLeft count xy = listIndices (\x -> (-x,0)) count xy
listIndicesHorzRight count xy = listIndices (\x -> (x,0)) count xy
listIndicesUpwardsLeft count xy = listIndices (\i -> (-i,i)) count xy
listIndicesUpwardsRight count xy = listIndices (\i -> (i,-i)) count xy
listIndicesDownwardsLeft count xy = listIndices (\i -> (-i,-i)) count xy
listIndicesDownwardsRight count xy = listIndices (\i -> (i,i)) count xy

listIndices :: (Int -> (Int, Int)) -> Int -> (Int, Int) -> [(Int, Int)]
listIndices f count (x,y) = map (\a -> let (u,v) = f a in (x + u, y + v)) (take count [1..])

filterOrdered :: Eq b => b -> [((Int, Int), Maybe b)] -> Maybe (Int, Int)
filterOrdered _ [] = Nothing
filterOrdered c ((a,b):[]) = case b of
    Nothing -> if inBoard a then Just a else Nothing
    Just c' -> if c == c' then Nothing else Just a
filterOrdered c ((_,b):cs) = case b of
    Nothing -> filterOrdered c cs
    Just c' -> if c == c'
        then filterOrdered c cs
        else Nothing

inBoard :: (Int, Int) -> Bool
inBoard (x,y) = x >= 0 && x <= 7 && y >= 0 && y <= 7

countFilledIndices :: Board -> [(Int, Int)] -> Int
countFilledIndices (Board m) = length . filter id . map (flip Map.member m)

verticalCount :: Board -> (Int, Int) -> Int
verticalCount b (x,_) = countFilledIndices b [(x,y) | y <- [0..7]]

horizontalCount :: Board -> (Int, Int) -> Int
horizontalCount b (_,y) = countFilledIndices b [(x,y) | x <- [0..7]]

-- Top left to bottom right
downwardsCount :: Board -> (Int, Int) -> Int
downwardsCount b (x,y) = countFilledIndices b $ zipWith
  (\u v -> (x + u, y + v))
  [(-7)..7]
  [(-7)..7]

-- Bottom left to top right
upwardsCount :: Board -> (Int, Int) -> Int
upwardsCount b (x,y) = countFilledIndices b $ zipWith
  (\u v -> (x + u, y + v))
  [7,6..(-7)]
  [(-7)..7]

opponent :: Checker -> Checker
opponent Checker'Black = Checker'White
opponent Checker'White = Checker'Black

terminal :: Board -> Maybe Terminal
terminal b = case (whiteWinner b, blackWinner b) of
    (True, True) -> Just Terminal'Draw
    (False, True) -> Just $ Terminal'Winner Checker'Black
    (True, False) -> Just $ Terminal'Winner Checker'White
    (False, False) -> Nothing

emptyBoard :: Board
emptyBoard = Board . Map.fromList $
    [ ((x,y), c) | x <- [1..6], let y = 0, let c = Checker'Black ] ++
    [ ((x,y), c) | x <- [1..6], let y = 7, let c = Checker'Black ] ++
    [ ((x,y), c) | y <- [1..6], let x = 0, let c = Checker'White ] ++
    [ ((x,y), c) | y <- [1..6], let x = 7, let c = Checker'White ]

whiteWinner :: Board -> Bool
whiteWinner = allConnected Checker'White

blackWinner :: Board -> Bool
blackWinner = allConnected Checker'Black

allConnected :: Checker -> Board -> Bool
allConnected c (Board b) = case headMay (Set.toList indices) of
        Just xy -> Set.size indices == Set.size (connected Set.empty indices xy)
        Nothing -> False
    where
        indices = Set.fromList $ Map.keys (Map.filter (== c) b)

neighbors :: Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
neighbors s (x,y) = Set.fromList $ filter (flip Set.member s)
    [ (x + x', y + y')
    | x' <- [(-1)..1]
    , y' <- [(-1)..1]
    , x' /= 0 || y' /= 0
    ]

connected :: Set.Set (Int, Int) -> Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
connected explored board xy = let
    ns = neighbors board xy
    frontier = Set.difference ns explored
    explored' = Set.union ns explored
    in if Set.null ns
        then Set.union explored' (Set.singleton xy)
        else foldr (\xy' b -> Set.union b (connected b board xy')) explored' (Set.toList frontier)
