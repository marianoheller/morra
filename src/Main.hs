module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy (StateT, get, runStateT)
import System.Random (Random (randomRIO))

type Score = (Integer, Integer)

type Play = (Integer, Integer)

type GameState = (Score, IO Play)

aiPlay :: IO Play
aiPlay = do
  hand <- randomRIO (1, 5)
  bet <- randomRIO (2, 10)
  return (hand, bet)

playerPlay :: IO Play
playerPlay = do
  putStrLn "Choose hand"
  playerHand <- liftIO getUserInput
  putStrLn "Choose expected score"
  playerBet <- liftIO getUserInput
  return (playerHand, playerBet)

reportPlay :: Play -> Play -> IO ()
reportPlay (handAI, betAI) (handPlayer, betPlayer) = do
  putStrLn "+++++++++"
  putStrLn $ "Computer bet is " ++ (show betAI)
  putStrLn $ "Computer hand is " ++ (show handAI)
  putStrLn "+++++++++"
  putStrLn $ "Player bet is " ++ (show betPlayer)
  putStrLn $ "Player hand is " ++ (show handPlayer)
  putStrLn "+++++++++"
  putStrLn $ "Full play value is " ++ (show $ handAI + handPlayer)

getUserInput :: IO Integer
getUserInput = fmap parseInput getLine
  where
    parseInput a = read a :: Integer

app :: StateT GameState IO GameState
app = do
  liftIO $ putStrLn "=========================="
  (score, ai) <- get
  computerChoice <- liftIO ai
  playerChoice <- liftIO playerPlay
  liftIO $ reportPlay computerChoice playerChoice
  get

main :: IO ()
main = do
  let state = ((0, 0), aiPlay)
  runStateT app state
  putStrLn ""
  putStrLn "GG!"
