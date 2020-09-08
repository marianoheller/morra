{-# LANGUAGE DeriveFunctor #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy (StateT, get, modify', runStateT)
import qualified Data.Bifunctor as Bi
import System.Random (Random (randomRIO))

data Player = Human | AI deriving (Eq, Show)

data SScore a = SScore Player a deriving (Show, Eq, Functor)

type Score = (SScore Integer, SScore Integer)

data Hand = Hand Integer

data Bet = Bet Integer

type Play = (Hand, Bet)

type GameState = (Score, IO Play)

aiPlay :: IO Play
aiPlay = do
  hand <- randomRIO (0, 5)
  bet <- randomRIO (0, 10)
  return (Hand hand, Bet bet)

playerPlay :: IO Play
playerPlay = do
  putStrLn "Choose hand"
  playerHand <- liftIO getUserInput
  putStrLn "Choose expected score"
  playerBet <- liftIO getUserInput
  return (Hand playerHand, Bet playerBet)
  where
    getUserInput = fmap parseInput getLine
    parseInput a = read a :: Integer

reportPlay :: Play -> Play -> StateT GameState IO ()
reportPlay (Hand handPlayer, Bet betPlayer) (Hand handAI, Bet betAI) = do
  let print = liftIO . putStrLn
  let hr = print "+++++++++"
  hr
  print $ "Computer bet is " ++ (show betAI)
  print $ "Computer hand is " ++ (show handAI)
  hr
  print $ "Player bet is " ++ (show betPlayer)
  print $ "Player hand is " ++ (show handPlayer)
  hr
  print $ "Full round value is " ++ (show $ handAI + handPlayer)
  (score, _) <- get
  print $ "Score value is " ++ (show score)

updateScore :: Play -> Play -> StateT GameState IO ()
updateScore (Hand handP, Bet betP) (Hand handAI, Bet betAI) = do
  let addPoint bet = fmap $ (+) (if bet == (handAI + handP) then 1 else 0)
  let _updateScore = Bi.first $ Bi.bimap (addPoint betP) (addPoint betAI)
  modify' _updateScore

runRound :: StateT GameState IO ()
runRound = do
  liftIO $ putStrLn "=========================="
  (score, ai) <- get
  computerChoice <- liftIO ai
  playerChoice <- liftIO playerPlay
  updateScore playerChoice computerChoice
  reportPlay playerChoice computerChoice
  return ()

evalGame :: StateT GameState IO (Maybe Player)
evalGame = do
  (score, _) <- get
  let f (SScore _ n) = (flip (>=) $ 3) n
  lift $ case Bi.bimap f f score of
    (True, _) -> return $ Just Human
    (_, True) -> return $ Just AI
    _ -> return Nothing

reportGame :: Player -> StateT GameState IO ()
reportGame player = do
  let playerToText p =
        case p of
          Human -> "Human"
          AI -> "Computer"
  let winner = playerToText player
  (score, _) <- get
  liftIO $ putStrLn $ "The winner was " ++ winner ++ " with score: " ++ (show score)

app :: StateT GameState IO ()
app = do
  runRound
  maybeWinner <- evalGame
  case maybeWinner of
    Just player -> reportGame player
    Nothing -> app

main :: IO ()
main = do
  let state = ((SScore Human 0, SScore AI 0), aiPlay)
  runStateT app state
  putStrLn ""
  putStrLn "GG!"
