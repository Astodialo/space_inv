{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Monad.Reader
import Control.Monad.State
import System.Console.ANSI
import System.Random
import Data.List.Unique
import System.IO

setNoBuffering :: IO ()
setNoBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

main :: IO ()
main = do
  setNoBuffering
  hideCursor
  config <- mkConfig
  game   <- mkGame
  print game
  runStateT (runReaderT renderGame config) game
  showCursor
  return ()

play :: ReaderT Config (StateT Game IO) ()
play = undefined 

type Size      = (Int, Int)
data Config    = Config { _size :: Size }

type Row       = Int
type Col       = Int
type Point     = (Int, Int)
newtype Craft  = Craft { _getSnake :: Point } deriving Show
newtype Aliens = Aliens { _getAliens :: [Point] } deriving Show
newtype Shots  = Shots { _getShots :: [Point] } deriving Show
data Direction = L | R | N  deriving Show
type Score     = Int
data Status    = On | Over deriving Show
data Game      = Game
  { _craft     :: Craft
  , _aliens    :: Aliens
  , _shots     :: Shots
  , _score     :: Score
  , _direction :: Direction
  , _status    :: Status
  } deriving Show 

mkConfig :: IO Config
mkConfig = do
  Just (mrow, mcol) <- getTerminalSize
  return $ Config { _size = (mrow, mcol)}

mkGame :: IO Game
mkGame = do
  Just (mRow, mCol) <- getTerminalSize
  let sRow = mRow `div` 2
  let sCol = mCol `div` 2
  aCols <- replicateM 10 $ randomRIO (0, mCol-1)
  aRows <- replicateM 10 $ randomRIO (0, 1)
  return $ Game
    { _craft     = Craft (sRow, sCol)
    , _aliens    = Aliens $ unique $ zip aRows aCols
    , _shots     = Shots [(sRow + 1, sCol)]
    , _score     = 0
    , _direction = N
    , _status    = On
    }

renderPoint :: (Row, Col) -> Char -> IO ()
renderPoint (r, c) x = setCursorPosition r c >> putChar x

renderGame :: ReaderT Config (StateT Game IO) ()
renderGame = do
  config <- ask
  game   <- get
  let (mrow, mcol) = _size config 
      aliens  = _aliens game
      Craft (crow, ccol) = _craft game
  liftIO (forM_ (_getAliens aliens) \(row, col) -> do
    setCursorPosition row col
    putChar '@')
  _ <- lift . lift $ getChar
  liftIO $ renderPoint (crow, ccol) '^'
