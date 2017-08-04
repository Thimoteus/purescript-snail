module Snail.Console where

import Prelude

import Control.Monad.Aff.Console (error, log)
import Snail.Types (Snail)

-- | Print a string to standard output
echo :: forall e. String -> Snail e String
echo s = log s *> pure s

-- | Print a string to standard error
err :: forall e. String -> Snail e String
err s = error s *> pure s

