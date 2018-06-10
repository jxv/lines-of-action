{-# LANGUAGE TupleSections #-}
module LinesOfAction where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Safe (headMay)

data Checker
    = Checker'Black
    | Checker'White
    deriving (Show, Eq)

newtype Board = Board (Map.Map (Int, Int) Checker)
    deriving (Show, Eq)

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
applyMove' board move@Move{moveFrom,moveTo} = do
    cells <- moveCells board moveFrom
    if elem moveTo cells
        then Just $ moveCheckerOnBoard board move
        else Nothing

moveCheckerOnBoard :: Board -> Move -> Board
moveCheckerOnBoard (Board m) move = Board
    $ Map.delete (moveFrom move)
    $ Map.insert (moveTo move) (moveChecker move) m

moveCells :: Board -> (Int, Int) -> Maybe [(Int, Int)]
moveCells b@(Board m) xy = do
  c <- Map.lookup xy m
  return $ eliagbleMoveCells b c xy

eliagbleMoveCells :: Board -> Checker -> (Int, Int) -> [(Int,Int)]
eliagbleMoveCells b c xy =
    filterOrdered c (lookupCells b $ listIndicesVert vertCount xy) ++
    filterOrdered c (lookupCells b $ listIndicesHorz horzCount xy) ++
    filterOrdered c (lookupCells b $ listIndicesUpwards upCount xy) ++
    filterOrdered c (lookupCells b $ listIndicesDownwards downCount xy)
  where
    vertCount = verticalCount b xy
    horzCount = horizontalCount b xy
    upCount = upwardsCount b xy
    downCount = downwardsCount b xy

lookupCells :: Board -> [(Int, Int)] -> [((Int, Int), Maybe Checker)]
lookupCells (Board m) xs = zip xs (map (flip Map.lookup m) xs)

listIndicesVert :: Int -> (Int, Int) -> [(Int, Int)]
listIndicesVert count xy = listIndices (\y -> (0,y)) count xy ++ listIndices (\y -> (0,-y)) count xy

listIndicesHorz :: Int -> (Int, Int) -> [(Int, Int)]
listIndicesHorz count xy = listIndices (\x -> (x,0)) count xy ++ listIndices (\x -> (-x,0)) count xy

listIndicesUpwards :: Int -> (Int, Int) -> [(Int, Int)]
listIndicesUpwards count xy = listIndices (\i -> (i,-i)) count xy ++ listIndices (\i -> (-i,i)) count xy

listIndicesDownwards :: Int -> (Int, Int) -> [(Int, Int)]
listIndicesDownwards count xy = listIndices (\i -> (i,i)) count xy ++ listIndices (\i -> (-i,-i)) count xy

listIndices :: (Int -> (Int, Int)) -> Int -> (Int, Int) -> [(Int, Int)]
listIndices f count (x,y) = filter inBoard $ map (\a -> let (u,v) = f a in (x + u, y + v)) (take count [1..])

filterOrdered :: Eq b => b -> [(a, Maybe b)] -> [a]
filterOrdered _ [] = []
filterOrdered c ((a,b):cs) = case b of
    Nothing -> a : filterOrdered c cs
    Just c' -> if c == c'
        then filterOrdered c cs
        else [a]

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
emptyBoard = Board $ Map.fromList $
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

neighbor :: Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
neighbor s (x,y) = Set.fromList $ filter (flip Set.member s)
    [ (x + x', y + y')
    | x' <- [(-1)..1]
    , y' <- [(-1)..1]
    , x' /= 0 || y' /= 0
    ]

connected :: Set.Set (Int, Int) -> Set.Set (Int, Int) -> (Int, Int) -> Set.Set (Int, Int)
connected explored board xy = let
    neighbors = neighbor board xy
    frontier = Set.difference neighbors explored
    explored' = Set.union neighbors explored
    in if Set.null neighbors
        then Set.union explored' (Set.singleton xy)
        else foldr (\xy' b -> Set.union b (connected b board xy')) explored' (Set.toList frontier)

-- | An example function.
main :: IO ()
main = do
    print emptyBoard
    return ()
