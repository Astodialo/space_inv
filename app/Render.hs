module Render (
  renderOver,
  renderNext,
  renderCraft
)

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import System.Console.ANSI

import Data
import Util

renderCharacter :: Char -> Point -> IO ()
renderCharacter c (y, x) = do
  setCursorPostion y x
  putChar c
  return ()

renderCraft :: Char -> Char -> Craft -> IO ()
renderCraft c d (Craft (p : ps)) = do
  renderCharacter c p
  mapM_ (renderCharacter d) ps


