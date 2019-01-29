module Data.GCode.Generate.ExamplesMonad where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

import Data.GCode
import Data.GCode.Monad
import Data.GCode.Generate

xprog = prog $ do
  move (x 10)
  rapid (xy 15 15)
  move (xy 0 0)
  coolantStop'

withState = prog $ do
  flip runStateT 0 $ do
    val <- get
    lift $ rapid (x val)
  forM_ (zip [0..10] [-10..0]) (move . uncurry xy)

