module Main where

import Control.Monad (unless, void, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)


-- Two player score
-- First player wants odd, second wants even
data Player = P1 | P2
  deriving (Eq, Enum)

type Pick = Int

data GameStatus = GameStatus
  { gsPickP1 :: Maybe Pick
  , gsPickP2 :: Maybe Pick
  , gsTurn :: Player
  } deriving (Eq, Show)

data MatchStatus = MatchStatus
  { scoreP1 :: Int
  , scoreP2 :: Int
  } deriving (Eq, Show)

type Game = StateT GameStatus IO
type Match = StateT MatchStatus IO

instance Show Player where
  show P1 = "Player"
  show P2 = "Computer"

getGameWinner :: GameStatus -> Maybe Player
getGameWinner (GameStatus (Just p1) (Just p2) _) =
  Just $ case (p1 + p2) `mod` 2 of
    1 -> P1
    0 -> P2
getGameWinner _ = Nothing

printGameWinner :: Game ()
printGameWinner = do
  w <- gets getGameWinner
  case w of
    Just p -> liftIO $ putStrLn $ show p ++ " wins this round."
    _      -> return ()

getMatchWinner :: MatchStatus -> Maybe Player
getMatchWinner (MatchStatus s1 s2) =
  case compare s1 s2 of
    LT -> Just P2
    EQ -> Nothing
    GT -> Just P1

printMatchWinner :: Match ()
printMatchWinner = do
  w <- gets getMatchWinner
  liftIO $ putStrLn $
    case w of
      Nothing -> "It's a tie!"
      Just p  -> show p ++ " wins!"

recordTurn :: Pick -> GameStatus -> GameStatus
recordTurn num (GameStatus p1 p2 t) =
  case t of
    P1 -> GameStatus (Just num) p2 P2
    P2 -> GameStatus p1 (Just num) P1

turn :: Game ()
turn = do
  t    <- gets gsTurn
  pick <- liftIO $ putStr (show t ++ ": ") >> hFlush stdout >> read <$> getLine
  modify (recordTurn pick)

game :: Game Player
game = do
  w <- gets getGameWinner
  case w of
    Nothing -> turn >> game
    Just p  -> printGameWinner >> return p

recordWin :: Player -> MatchStatus -> MatchStatus
recordWin p (MatchStatus s1 s2) =
  case p of
    P1 -> MatchStatus (succ s1) s2
    P2 -> MatchStatus s1 (succ s2)

match :: Bool -- ^ Initial game flag
      -> Match ()
match initial = do
  unless initial $ do
    cont <- liftIO continue
    unless cont $ printMatchWinner >> liftIO exitSuccess
  forever $ do
    (w, _) <- liftIO $ runStateT game (GameStatus Nothing Nothing P1)
    modify (recordWin w)
    match False
  
continue :: IO Bool
continue = do
  putStr "Continue [y/n]? "
  hFlush stdout
  input <- getLine
  return $ any isYes input
    where isYes = (||) <$> (=='y') <*> (=='Y')

main :: IO ()
main = void $ runStateT (match True) (MatchStatus 0 0)
