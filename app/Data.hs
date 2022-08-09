module Data (
  Config (..),
  Direction,
  Event (..),
  Alien (..),
  Game (..),
  Point (..),
  Score,
  Size,
  Craft (..),
  Time,
  Shot
) where

import Control.Monad.State
import Control.Monad.Reader

date Event
  = TickEvent
  | KeyEvent
  deriving Show

type Size      = (Int, Int)
type Time      = Int
data Config    = Config {
  
} deriving Show

type Point     = (Int, Int)
newtype Craft  = Craft { getCraft :: [Point] } deriving Show
newtype Alien  = Alien { getAlien :: Point } deriving Show
newtype Shot   = Shot  { getShot  :: Point } deriving Show
type Direction = String
type Score     = Int
data Game      = Game {
  craft       :: Craft,
  aliens      :: [Alien],
  shots       :: [Shot]
  direction   :: Direction,
  score       :: Int
} deriving Show
