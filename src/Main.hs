module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy (StateT, get, modify', runStateT)
import qualified Data.Bifunctor as Bi
import System.Random (Random (randomRIO))

data Player = Human | AI

type Score = (Integer, Integer)

type Play = (Integer, Integer)

type GameState = (Score, IO Play)

aiPlay :: IO Play
aiPlay = do
  hand <- randomRIO (0, 5)
  bet <- randomRIO (0, 10)
  return (hand, bet)

playerPlay :: IO Play
playerPlay = do
  putStrLn "Choose hand"
  playerHand <- liftIO getUserInput
  putStrLn "Choose expected score"
  playerBet <- liftIO getUserInput
  return (playerHand, playerBet)
  where
    getUserInput = fmap parseInput getLine
    parseInput a = read a :: Integer

reportPlay :: Play -> Play -> StateT GameState IO ()
reportPlay (handPlayer, betPlayer) (handAI, betAI) = do
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
updateScore (handPlayer, betPlayer) (handAI, betAI) = do
  let addPoint bet = (+)  (if bet == (handAI + handPlayer) then 1 else 0)
  let _updateScore = Bi.first $ \(ai, player) -> (addPoint betPlayer player, addPoint betAI ai)
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
  let f = (flip (>=) $ 3)
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
  liftIO $ putStrLn $ "The winner was " ++ winner ++ "with score: " ++ (show score)

app :: StateT GameState IO ()
app = do
  runRound
  maybeWinner <- evalGame
  case maybeWinner of
    Just player -> reportGame player
    Nothing -> app

main :: IO ()
main = do
  let state = ((0, 0), aiPlay)
  runStateT app state
  putStrLn ""
  putStrLn "GG!"
