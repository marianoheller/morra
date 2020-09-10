{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE KindSignatures #-}

module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State.Lazy (StateT, get, modify', runStateT)
import qualified Data.Bifunctor as Bi
import System.Random (Random (randomRIO))

data Player = Human | AI deriving (Eq, Show)

data SScore a = SScore Player a deriving (Show, Eq, Functor)

type Score = (SScore Integer, SScore Integer)

data Hand = Hand Integer

data Bet = Bet Integer

data Play (a :: Player) = Play (Hand, Bet)

type Round = (Play Human, Play AI)

type GameState = (Score, [Round])

calcExpectedHand :: [Round] -> Maybe Integer
calcExpectedHand [] = Nothing
calcExpectedHand rounds = return $ go rounds 0 0
  where
    go [] acc n = acc `div` n
    go _ acc 3 = acc `div` 3
    go ((Play a, _) : xs) acc n = go xs (acc + h1) (n + 1)
      where
        (Hand h1, Bet _) = a

aiPlay :: StateT GameState IO (Play AI)
aiPlay = do
  (_, a) <- get
  let calced = calcExpectedHand a
  expected <-
    case calced of
      Nothing -> liftIO $ randomRIO (0, 5)
      Just a -> return a
  hand <- liftIO $ randomRIO (0, 5)
  let bet = hand + expected
  return $ Play (Hand hand, Bet bet)

playerPlay :: IO (Play Human)
playerPlay = do
  putStrLn "Choose hand"
  playerHand <- liftIO getUserInput
  putStrLn "Choose expected score"
  playerBet <- liftIO getUserInput
  return $ Play (Hand playerHand, Bet playerBet)
  where
    getUserInput = fmap parseInput getLine
    parseInput a = read a :: Integer

reportPlay :: Round -> StateT GameState IO ()
reportPlay (Play (Hand handPlayer, Bet betPlayer), Play (Hand handAI, Bet betAI)) = do
  let print = liftIO . putStrLn
  let hr = print "+++++++++"
  (score, _) <- get
  hr
  print $ "Computer bet is " ++ (show betAI)
  print $ "Computer hand is " ++ (show handAI)
  hr
  print $ "Player bet is " ++ (show betPlayer)
  print $ "Player hand is " ++ (show handPlayer)
  hr
  print $ "Full round value is " ++ (show $ handAI + handPlayer)
  print $ "Score value is " ++ (show score)

updateState :: Round -> StateT GameState IO ()
updateState r@(Play (Hand handP, Bet betP), Play (Hand handAI, Bet betAI)) = do
  let addPoint bet = fmap $ (+) (if bet == (handAI + handP) then 1 else 0)
  let updateScore = Bi.bimap (addPoint betP) (addPoint betAI)
  let updateRounds rounds = r : rounds
  let updateInternalState = Bi.bimap updateScore updateRounds
  modify' updateInternalState

runRound :: StateT GameState IO ()
runRound = do
  liftIO $ putStrLn "=========================="
  computerChoice <- aiPlay
  playerChoice <- liftIO playerPlay
  updateState (playerChoice, computerChoice)
  reportPlay (playerChoice, computerChoice)

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

morra :: StateT GameState IO ()
morra = do
  runRound
  maybeWinner <- evalGame
  case maybeWinner of
    Just player -> reportGame player
    Nothing -> morra

main :: IO ()
main = do
  let state = ((SScore Human 0, SScore AI 0), [])
  runStateT morra state
  putStrLn "GG! =)"
