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

import Data.Either (either)
import Effect.Aff (runAff)
import Effect.Class.Console (error)
import Effect.Exception (message)
import Snail.Console as Console
import Snail.Control as Control
import Snail.Env as Env
import Snail.File as File
import Snail.OS as OS
import Snail.Path as Path
import Snail.Process as Process
import Snail.Types as Types

-- | Runs a Snail computation
crawl :: forall a. Types.Snail a -> Types.Script Unit
crawl snail = void (runAff (either (error <<< message) (const (pure unit))) snail)

infixl 4 bind as |>
