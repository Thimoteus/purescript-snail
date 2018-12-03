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
import Snail.Console (echo, err) as Console
import Snail.Control (andandand, exists, existsCheck, fromEither, fromJust, fromMaybe, loop, mIf, notExistsCheck, ororor, sleep, when, (&&&), (|||), (~?>), (~~>)) as Control
import Snail.Env (Optional(..), getVar, printEnv, setVar) as Env
import Snail.File (appendFile, cat, chmod, cp, ls, mkdir, mv, prependFile, rm, rmdir, touch, writeFile, (+>), (>>)) as File
import Snail.OS (arch, cpus, endianness, freemem, home, hostname, loadavg, networkInterfaces, ostype, platform, release, tmp, totalmem, uptime) as OS
import Snail.Path (chdir, pathpend, (</>)) as Path
import Snail.Process (args, exec, exit, exitWith, fork, input, params, raw, run, (!?)) as Process
import Snail.Types (class Address, FILE, FOLDER, File, Folder, Script, Snail, file, folder, getAddress, runFile, runFolder) as Types

-- | Runs a Snail computation
crawl :: forall a. Types.Snail a -> Types.Script Unit
crawl snail = void (runAff (either (error <<< message) (const (pure unit))) snail)

infixl 4 bind as |>
