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
moveAl = map (\(x,y) -> (x + 1, y))

moveAliens :: (MonadState Game m) => m ()
moveAliens = do -- aliens . getAliens %= moveAl 
  game <- get
  let naliens = _getAliens (_aliens game)
  aliens .= Aliens (moveAl naliens)
  -- aliens . getAliens .= moveAl (game ^. aliens . getAliens)
  -- aliens . getAliens .= moveAl naliens

newAliens :: (MonadIO m, MonadState Game m, MonadReader Config m) => m()
newAliens = do
  config <- ask
  game <- get
  cAliens <- gets (_getAliens . _aliens)
  let (mrow, mcol) = _size config
  aCols <- replicateM 10 $ randomRIO (1, mcol-2)
  aRows <- replicateM 10 $ randomRIO (2, 2)
  let nAliens = Aliens $ unique $ zip aRows aCols
  aliens .= Aliens (cAliens ++ _getAliens  nAliens)

moveSh :: [Point] -> [Point]
moveSh = map (\(x,y) -> (x - 1, y))

moveShots :: (MonadState Game m) => m ()
moveShots = do
      game <- get
      let nshots = _getShots (_shots game)
      shots .= Shots (moveSh nshots)

newShot :: (MonadState Game m) => m ()
newShot = do
  ccraft <- gets (_getCraft . _craft)
  cshots <- gets (_getShots . _shots)
  let  nshot = moveSh [ccraft] 
  shots .= Shots (nshot ++ cshots)

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
  moveAliens
  moveShots
  newShot
  newAliens

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

