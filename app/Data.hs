{-# LANGUAGE BlockArguments, TemplateHaskell, FlexibleContexts #-}

module Data (
  Config (..),
  Direction (..),
  direction,
  Point,
  Craft (..),
  craft,
  Aliens (..),
  aliens,
  Shots (..),
  shots,
  Game (..),
  Score,
  Status (..),
) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import Control.Lens.TH

type Size      = (Int, Int)
data Config    = Config { _size :: Size }
makeLenses ''Config

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


