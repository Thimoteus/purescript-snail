module Snail.Process
  ( exec
  , raw
  , fork
  , run
  , args
  , params
  , input
  , exit
  , exitWith, (!?)
  ) where

import Prelude

import Control.Monad.Aff (Aff, apathize, effCanceler, forkAff, makeAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (throwException)
import Data.Array (drop, head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Node.Buffer (toString)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.Process as Process
import Snail.Console (echo, err)
import Snail.Control (fromJust, (&&&))
import Snail.Types (Snail, Script)

execFileImp :: forall e. String -> Array String -> Aff ( cp :: CP.CHILD_PROCESS | e ) CP.ExecResult
execFileImp cmd as = makeAff execFileImpl
  where
    execFileImpl cb =
      let
        onRes res = case res.error of
          Just x -> cb (Left x)
          _ -> cb (Right res)
      in
        pure (effCanceler (CP.execFile cmd as CP.defaultExecOptions onRes))

-- | Run a command with an array of arguments, getting the output as a UTF8
-- | encoded string. Note that this uses node's `execFile` and not `exec` under
-- | the hood.
exec :: forall e. NonEmpty Array String -> Snail e String
exec (cmd :| as) = do
  res <- execFileImp cmd as
  liftEff $ toString UTF8 res.stdout

rawImpl :: forall e. String -> Snail e CP.ExecResult
rawImpl cmd = makeAff rawImpl'
  where
    rawImpl' cb =
      let
        onRes res = case res.error of
          Just x -> cb (Left x)
          _ -> cb (Right res)
      in
        pure (effCanceler (CP.exec cmd CP.defaultExecOptions onRes))

-- | Runs a raw command using node's `exec`.
raw :: forall e. String -> Snail e String
raw cmd = do
  res <- rawImpl cmd
  liftEff $ toString UTF8 res.stdout

foreign import unref :: forall e. CP.ChildProcess -> Script e Unit

-- | Send a process into the background.
fork :: forall e. NonEmpty Array String -> Snail e Unit
fork (cmd :| as)
  = apathize <<< forkAff <<< liftEff <<< unref
  <=< liftEff
    $ CP.spawn cmd as (CP.defaultSpawnOptions {stdio = CP.ignore, detached = true})

-- Run a command as a child process, inheriting stdout, stderr and stdin from it
run :: forall e. NonEmpty Array String -> Snail e Unit
run (cmd :| as) = do
  cp <- liftEff $ CP.spawn cmd as $ CP.defaultSpawnOptions {stdio = CP.inherit}
  liftEff $ CP.onError cp $ throwException <<< CP.toStandardError

-- | Get all the arguments to the script.
args :: forall e. Snail e (Array String)
args = liftEff Process.argv

-- | Get the arguments to the script, minus the first two, the first of which is
-- | always "node", the second is the name of the file being executed.
params :: forall e. Snail e (Array String)
params = drop 2 <$> args

-- | Get the first non-mandatory argument, which must have necessarily been
-- | provided.
input :: forall e. Snail e String
input = head <$> params >>= fromJust "No input provided"

-- | Exit the script with the given exit code.
exit :: forall a e. Int -> Snail e a
exit = liftEff <<< Process.exit

-- | Exit the script with the given exit code, printing the given message to
-- | standard output if the exit code is 0, and standard error otherwise.
exitWith :: forall a e. Int -> String -> Snail e a
exitWith 0 msg = echo msg &&& exit 0
exitWith code msg = err msg &&& exit code

infix 5 exitWith as !?
