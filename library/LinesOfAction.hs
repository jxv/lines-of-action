module LinesOfAction where

import qualified Data.Map as Map

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

-- | An example function.
main :: IO ()
main = do
    print emptyBoard
    return ()