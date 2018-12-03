module Snail.Console where

import Prelude

import Effect.Aff (Aff)
import Effect.Class.Console (error, log)

-- | Print a string to standard output
echo :: String -> Aff String
echo s = log s *> pure s

-- | Print a string to standard error
err :: String -> Aff String
err s = error s *> pure s

