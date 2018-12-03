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

import Data.Array (drop, head)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, (:|))
import Effect (Effect)
import Effect.Aff (Aff, apathize, effectCanceler, forkAff, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (throwException)
import Node.Buffer (toString)
import Node.ChildProcess as CP
import Node.Encoding (Encoding(..))
import Node.Process as Process
import Snail.Console (echo, err)
import Snail.Control (fromJust, (&&&))

execFileImp :: String -> Array String -> Aff CP.ExecResult
execFileImp cmd arguments = makeAff execFileImpl
  where
    execFileImpl cb =
      let
        onRes res = case res.error of
          Just x -> cb (Left x)
          _ -> cb (Right res)
      in
        pure (effectCanceler (void $ CP.execFile cmd arguments CP.defaultExecOptions onRes))

-- | Run a command with an array of arguments, getting the output as a UTF8
-- | encoded string. Note that this uses node's `execFile` and not `exec` under
-- | the hood.
exec :: NonEmpty Array String -> Aff String
exec (cmd :| as) = do
  res <- execFileImp cmd as
  liftEffect $ toString UTF8 res.stdout

rawImpl :: String -> Aff CP.ExecResult
rawImpl cmd = makeAff rawImpl'
  where
    rawImpl' cb =
      let
        onRes res = case res.error of
          Just x -> cb (Left x)
          _ -> cb (Right res)
      in
        pure (effectCanceler (void $ CP.exec cmd CP.defaultExecOptions onRes))

-- | Runs a raw command using node's `exec`.
raw :: String -> Aff String
raw cmd = do
  res <- rawImpl cmd
  liftEffect $ toString UTF8 res.stdout

foreign import unref :: CP.ChildProcess -> Effect Unit

-- | Send a process into the background.
fork :: NonEmpty Array String -> Aff Unit
fork (cmd :| as)
  = apathize <<< forkAff <<< liftEffect <<< unref
  <=< liftEffect
    $ CP.spawn cmd as (CP.defaultSpawnOptions {stdio = CP.ignore, detached = true})

-- Run a command as a child process, inheriting stdout, stderr and stdin from it
run :: NonEmpty Array String -> Aff Unit
run (cmd :| as) = do
  cp <- liftEffect $ CP.spawn cmd as $ CP.defaultSpawnOptions {stdio = CP.inherit}
  liftEffect $ CP.onError cp $ throwException <<< CP.toStandardError

-- | Get all the arguments to the script.
args :: Aff (Array String)
args = liftEffect Process.argv

-- | Get the arguments to the script, minus the first two, the first of which is
-- | always "node", the second is the name of the file being executed.
params :: Aff (Array String)
params = drop 2 <$> args

-- | Get the first non-mandatory argument, which must have necessarily been
-- | provided.
input :: Aff String
input = head <$> params >>= fromJust "No input provided"

-- | Exit the script with the given exit code.
exit :: forall a. Int -> Aff a
exit = liftEffect <<< Process.exit

-- | Exit the script with the given exit code, printing the given message to
-- | standard output if the exit code is 0, and standard error otherwise.
exitWith :: forall a. Int -> String -> Aff a
exitWith 0 msg = echo msg &&& exit 0
exitWith code msg = err msg &&& exit code

infix 5 exitWith as !?
