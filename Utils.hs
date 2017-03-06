module Utils where

import Control.Monad
import Control.Applicative

guardM :: (Alternative m, Monad m) => m Bool -> m ()
guardM mb = do
    b <- mb
    guard b
