module Data.GCode.Generate.ExamplesMonad where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import Data.GCode.Generate (x, xy)
import Data.GCode.Monad

xprog :: Program
xprog = prog $ do
  move (x 10)
  rapid (xy 15 15)
  move (xy 0 0)
  coolantStop'

withState :: Program
withState = prog $ do
  void $ flip runStateT 0 $ do
    val <- get
    lift $ rapid (x val)
  forM_ (zip [0..10] [-10..0]) (move . uncurry xy)

