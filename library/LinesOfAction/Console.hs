{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module LinesOfAction.Console where

import Control.Monad.State
import Safe (readMay)

import LinesOfAction

newtype Console a = Console (StateT Board IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

runConsole :: Console a -> Board -> IO a
runConsole (Console m) b = evalStateT m b

instance Game Console where
    getMove b c = do
        liftIO $ putStrLn $ case c of
            Checker'White -> "Turn: White"
            Checker'Black -> "Turn: Black"
        liftIO $ print b
        liftIO $ putStr "Input (eg. 1,1 2,0): "
        line <- liftIO getLine
        case line of
            x0:',':y0:' ':x1:',':y1:[] -> let
                from = (,) <$> readMay [x0] <*> readMay [y0]
                to = (,) <$> readMay [x1] <*> readMay [y1]
                in case (,) <$> from <*> to of
                    Nothing -> do
                        liftIO $ putStrLn "Unable to read"
                        getMove b c
                    Just (from',to') -> return $ Move from' to' c
            _ -> do
                liftIO $ putStrLn "Unable to read"
                getMove b c
    invalidateMove b c _ = do
        liftIO $ putStrLn "Invalid move"
        play b c
    validatedMove _ b _ _ = liftIO $ print b
    terminate b t = do
        liftIO $ print b
        liftIO . putStrLn $ case t of
            Terminal'Draw -> "Draw!"
            Terminal'Winner c -> case c of
                Checker'White -> "White Wins!"
                Checker'Black -> "Black Wins!"


main :: IO ()
main = runConsole startGame emptyBoard