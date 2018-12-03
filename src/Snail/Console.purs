module Snail.Console where

import Prelude

import Effect.Class.Console (error, log)
import Snail.Types (Snail)

-- | Print a string to standard output
echo :: String -> Snail String
echo s = log s *> pure s

-- | Print a string to standard error
err :: String -> Snail String
err s = error s *> pure s

