module Main where

import Control.Monad (unless, void, forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State
import Data.Bool (bool)
import System.Exit (exitSuccess)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)
import System.Console.ANSI (clearScreen)

-- Two player score
-- First player wants odd, second wants even
data Player = P1 | P2
  deriving (Eq, Show, Enum)

-- Determines the type of the second player
data P2Type = Human | Computer
  deriving (Eq, Show)

type Pick = Int

data GameState = GameState
  { gPickP1 :: Maybe Pick
  , gPickP2 :: Maybe Pick
  , gTurn   :: Player
  , gType   :: P2Type
  , scoreP1 :: Int
  , scoreP2 :: Int
  } deriving (Eq, Show)

type Game = StateT GameState IO

getGameWinner :: GameState -> Maybe Player
getGameWinner (GameState (Just p1) (Just p2) _ _ _ _) =
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

getMatchWinner :: GameState -> Maybe Player
getMatchWinner (GameState _ _ _ _ s1 s2) =
  case compare s1 s2 of
    LT -> Just P2
    EQ -> Nothing
    GT -> Just P1

printMatchWinner :: Game ()
printMatchWinner = do
  w <- gets getMatchWinner
  liftIO $ putStrLn $
    case w of
      Nothing -> "It's a tie!"
      Just p  -> show p ++ " wins!"

recordTurn :: Pick -> GameState -> GameState
recordTurn num g =
  case gTurn g of
    P1 -> g { gPickP1 = Just num, gTurn = P2 }
    P2 -> g { gPickP2 = Just num, gTurn = P1 }

turn :: Game ()
turn = do
  p <- gets gTurn
  t <- gets gType
  liftIO $ putStr (show p ++ ": ") >> hFlush stdout
  pick <- if (p == P2 && t == Computer)
             then getCompPick
             else getHumanPick
  modify (recordTurn pick)

-- Be fancy - only clear screen when there are two humans
getHumanPick :: Game Pick
getHumanPick = do
  t <- gets gType
  x <- liftIO $ read <$> getLine
  when (t == Human) $ liftIO clearScreen
  return x

getCompPick :: Game Pick
getCompPick = liftIO $ do
  x <- randomRIO (1,1000)
  print x
  return x

game :: Game Player
game = do
  w <- gets getGameWinner
  case w of
    Nothing -> turn >> game
    Just p  -> printGameWinner >> return p

recordWin :: Player -> GameState -> GameState
recordWin p g@(GameState _ _ _ _ s1 s2) =
  case p of
    P1 -> g { scoreP1 = succ s1 }
    P2 -> g { scoreP2 = succ s2 }

resetGame :: GameState -> GameState
resetGame g = g { gPickP1 = Nothing, gPickP2 = Nothing, gTurn = P1 }

match :: Bool -- ^ Initial game flag
      -> Game ()
match initial = do
  unless initial $ do
    cont <- liftIO continue
    unless cont $ printMatchWinner >> liftIO exitSuccess
  forever $ do
    modify resetGame
    w <- game
    modify (recordWin w)
    match False

prompt :: String -> IO Bool
prompt p = do
  putStr $ p ++ " [y/n]? "
  hFlush stdout
  input <- getLine
  return $ any isYes input
    where isYes = (||) <$> (=='y') <*> (=='Y')

continue :: IO Bool
continue = prompt "Continue"

promptType :: IO P2Type
promptType =
  bool Human Computer
    <$> prompt "Play computer?"

main :: IO ()
main = do
  putStrLn "Welcome to Morra!"
  ptype <- promptType
  void $ runStateT (match True)
    (GameState Nothing Nothing P1 ptype 0 0)
