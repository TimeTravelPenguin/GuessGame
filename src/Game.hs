{-# LANGUAGE BlockArguments #-}
module Game (newGameStart) where

import           Control.Monad.RWS.Strict (MonadIO (liftIO), MonadReader (ask),
                                           RWST, execRWST, modify', unless)
import           System.IO                (hFlush, stdout)
import           System.Random.Stateful   (applyAtomicGen, globalStdGen,
                                           uniformR)
import           Text.Read                (readMaybe)

type SecretNumber = Integer
type GuessState = Integer

type GameM m a = RWST SecretNumber () GuessState m a

guessHint :: Ordering -> String
guessHint LT = "Higher..."
guessHint GT = "Smaller..."
guessHint EQ = "You got it!"

incrementGuessState :: GameM IO ()
incrementGuessState = do
  modify' (+ 1)
  return ()

checkGuess :: Integer -> GameM IO Bool
checkGuess guess = do
  incrementGuessState
  secretNumber <- ask
  let ord = compare guess secretNumber
  liftIO $ putStrLn (guessHint ord) >> hFlush stdout
  return $ ord == EQ

attemptGuess :: GameM IO Bool
attemptGuess = do
  liftIO $ putStr "\nGuess my secret number between 1-100: " >> hFlush stdout
  guess <- liftIO (readMaybe <$> getLine :: IO (Maybe Integer))
  let err = liftIO $ putStrLn "Invalid input, try again..."
  maybe (err >> attemptGuess) checkGuess guess

gameLoop :: GameM IO ()
gameLoop = do
  guess <- attemptGuess
  unless guess gameLoop

mkRandomInteger :: IO SecretNumber
mkRandomInteger = do
  applyAtomicGen (uniformR (1, 100)) globalStdGen

newGameStart :: IO ()
newGameStart = do
  secretNumber <- mkRandomInteger
  (guesses, _) <- execRWST gameLoop secretNumber 0
  putStrLn $ unwords ["\nIt took you", show guesses, "guesses!"]
