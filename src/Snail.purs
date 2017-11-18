module Snail
  ( module Types
  , module OS
  , module Path
  , module Console
  , module Control
  , module Process
  , module Env
  , module File
  , crawl
  , (|>)
  ) where

import Prelude

import Control.Monad.Aff (runAff)
import Control.Monad.Eff.Console (error)
import Control.Monad.Eff.Exception (message)
import Data.Either (either)
import Snail.Console as Console
import Snail.Control as Control
import Snail.Env as Env
import Snail.File as File
import Snail.OS as OS
import Snail.Path as Path
import Snail.Process as Process
import Snail.Types as Types

-- | Runs a Snail computation
crawl :: forall e a. Types.Snail e a -> Types.Script e Unit
crawl snail = void (runAff (either (error <<< message) (const (pure unit))) snail)

infixl 4 bind as |>
