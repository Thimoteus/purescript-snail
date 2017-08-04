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
import Snail.OS as OS
import Snail.Path as Path
import Snail.Types as Types
import Snail.Console as Console
import Snail.Control as Control
import Snail.Process as Process
import Snail.Env as Env
import Snail.File as File

-- | Runs a Snail computation
crawl :: forall e a. Types.Snail e a -> Types.Script e Unit
crawl = void <<< runAff (error <<< message) \ _ -> pure unit

infixl 4 bind as |>
