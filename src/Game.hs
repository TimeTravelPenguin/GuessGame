{-# LANGUAGE BlockArguments #-}
module Game (newGameStart) where

import           Control.Monad.State.Strict (MonadIO (..),
                                             MonadState (get, put), StateT,
                                             execStateT, unless)
import           System.IO                  (hFlush, stdout)
import           System.Random.Stateful     (applyAtomicGen, globalStdGen,
                                             uniformR)
import           Text.Read                  (readMaybe)

data GameState = GameState Integer Integer

type GameM m a = StateT GameState m a

giveHint :: MonadIO m => Ordering -> m ()
giveHint ord = do
  liftIO $ putStrLn
    case ord of
      LT -> "Higher..."
      GT -> "Smaller..."
      EQ -> "You got it!"
    >> hFlush stdout
  return ()

incrementGuessState :: GameM IO ()
incrementGuessState = do
  (GameState s g) <- get
  put (GameState s (g + 1))
  return ()

checkGuess :: Integer -> GameM IO Bool
checkGuess g = do
  incrementGuessState
  (GameState s _) <- get
  let ord = compare g s
  giveHint ord
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

mkRandomNewGame :: IO GameState
mkRandomNewGame = do
  rng <- applyAtomicGen (uniformR (1, 100)) globalStdGen :: IO Integer
  return (GameState rng 0)

newGameStart :: IO ()
newGameStart = do
  gameState <- mkRandomNewGame
  (GameState _ guesses) <- execStateT gameLoop gameState
  putStrLn $ unwords ["\nIt took you", show guesses, "guesses!"]
