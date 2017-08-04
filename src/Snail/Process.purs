module Snail.Process where

import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff.Class (liftEff)
import Data.Array (drop, head)
import Data.Maybe (Maybe(..))
import Node.Buffer (toString)
import Node.ChildProcess (CHILD_PROCESS, ChildProcess, ExecResult, defaultExecOptions, defaultSpawnOptions, execFile, ignore, spawn)
import Node.Encoding (Encoding(..))
import Node.Process as Process
import Snail.Console (echo, err)
import Snail.Control (fromJust, (&&&))
import Snail.Types (Snail, Script)

runFileImp :: forall e. String -> Array String -> Aff ( cp :: CHILD_PROCESS | e ) ExecResult
runFileImp cmd as = makeAff execFileImpl
  where
    execFileImpl fc sc =
      let onRes res = case res.error of
            Just x -> fc x
            _ -> sc res
       in execFile cmd as defaultExecOptions onRes

-- | Run a command with an array of arguments, getting the output as a UTF8
-- | encoded string.
run :: forall e. String -> Array String -> Snail e String
run cmd as = do
  res <- runFileImp cmd as
  liftEff $ toString UTF8 res.stdout

-- | Run a command, disregarding the output.
command :: forall e. String -> Array String -> Snail e Unit
command cmd = void <<< run cmd

foreign import unref :: forall e. ChildProcess -> Script e Unit

-- | Send a process into the background.
fork :: forall e. String -> Array String -> Snail e Unit
fork cmd as = liftEff <<< unref
          <=< liftEff $ spawn cmd as (defaultSpawnOptions {stdio = ignore, detached = true})

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
