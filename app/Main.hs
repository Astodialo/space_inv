{-# LANGUAGE BlockArguments, TemplateHaskell, FlexibleContexts #-}

module Main where

import Control.Monad.Reader
import Control.Monad.State
import System.Console.ANSI
import Control.Concurrent
import System.Random
import Data.List.Unique
import System.IO
import Control.Lens
import Control.Lens.TH

setNoBuffering :: IO ()
setNoBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

type Size      = (Int, Int)
data Config    = Config { _size :: Size }
makeLenses ''Config

type Row       = Int
type Col       = Int
type Point     = (Int, Int)
newtype Craft  = Craft { _getCraft :: Point } deriving Show
makeLenses ''Craft

newtype Aliens = Aliens { _getAliens :: [Point] } deriving Show
makeLenses '' Aliens

newtype Shots  = Shots { _getShots :: [Point] } deriving Show
makeLenses '' Shots

data Direction = L | R deriving Show
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

makeLenses ''Game

mkConfig :: IO Config
mkConfig = do
  Just (mrow, mcol) <- getTerminalSize
  return $ Config { _size = (mrow, mcol)}

mkGame :: IO Game
mkGame = do
  Just (mRow, mCol) <- getTerminalSize
  let sRow = mRow 
  let sCol = mCol `div` 2
  aCols <- replicateM 10 $ randomRIO (1, mCol-2)
  aRows <- replicateM 10 $ randomRIO (2, 2)
  return $ Game
    { _craft     = Craft (sRow - 2, sCol)
    , _aliens    = Aliens $ unique $ zip aRows aCols
    , _shots     = Shots [(sRow - 3, sCol)]
    , _score     = 0
    , _direction = R
    , _status    = On
    }

-- isValidDirection :: (MonadState m) => Direction -> m Bool

chDirec :: (MonadState Game m) => Direction -> m ()
chDirec d1= do
  d0 <- fmap _direction get
  direction .= d1

moveCr :: Direction -> Point -> Point
moveCr L (r, c) = (r, c - 1)
moveCr R (r, c) = (r, c + 1)

moveCraft :: MonadState Game m => Direction -> m ()
moveCraft d = do
  game <- get
  let Craft (row, col) = _craft game
      newC = moveCr d (row, col)
  put game { _craft = Craft newC }

moveAl :: [Point] -> [Point]
moveAl [] = []
moveAl ((x, y) : xs)= (x, y - 1) : moveAl xs 

moveAliens :: (MonadState Game m) => m ()
moveAliens = do
  game <- get
  let naliens (x:xs) = _getAliens (_aliens game)
      naliens [] = [] 
      naliens (x:xs) = do
        moveAl x
        naliens xs
  aliens .= naliens
   
  
renderGame :: (MonadIO m, MonadReader Config m, MonadState Game m) => m ()
renderGame = do
  config <- ask
  game   <- get
  let (mrow, mcol) = _size config 
      aliens  = _aliens game
      Craft (crow, ccol) = _craft game
      shots    = _shots game
      renderPoint x (r, c) = setCursorPosition r c >> putChar x
  liftIO clearScreen
  liftIO (forM_ (zip (repeat 1) [0..mcol]) (renderPoint '#')) 
  liftIO (forM_ (zip (repeat (mrow - 1)) [0..mcol]) (renderPoint '#')) 
  liftIO (forM_ (zip [1..mrow] (repeat 0)) (renderPoint '#')) 
  liftIO (forM_ (zip [1..mrow] (repeat (mcol - 1))) (renderPoint '#')) 
  liftIO (forM_ (_getAliens aliens) \(row, col) -> renderPoint '@' (row, col))   
  liftIO $ renderPoint 'W' (crow, ccol)
  liftIO (forM_ (_getShots shots) \(row, col) -> renderPoint '|' (row, col))
  liftIO $ setCursorPosition 0 (mcol - 3)
  liftIO (print $ _score game)
  return ()

play :: ReaderT Config (StateT Game IO) ()
play = forever $ do
  renderGame
  c <- lift . lift $ getChar
  case c of 
    'a' -> moveCraft L
    'd' -> moveCraft R

main :: IO ()
main = do
  setNoBuffering
  hideCursor
  config <- mkConfig
  game   <- mkGame
  runStateT (runReaderT play config) game
  _ <- liftIO $ getChar
  showCursor
  return ()

