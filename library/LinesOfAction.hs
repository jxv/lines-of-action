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

emptyBoard :: Board
emptyBoard = Board $ Map.fromList $
    [ ((x,y), c) | x <- [1..6], let y = 0, let c = Checker'Black ] ++
    [ ((x,y), c) | x <- [1..6], let y = 7, let c = Checker'Black ] ++
    [ ((x,y), c) | y <- [1..6], let x = 0, let c = Checker'White ] ++
    [ ((x,y), c) | y <- [1..6], let x = 7, let c = Checker'White ]

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