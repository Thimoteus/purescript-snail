module Snail
  ( module T
  , module OS
  , module P
  , crawl
  , command
  , run
  , fork
  , echo
  , err
  , (|>)
  , sleep
  , loop
  , cat
  , ls
  , appendFile, (>>)
  , writeFile, (+>)
  , prependFile
  , andandand, (&&&)
  , ororor, (|||)
  , exists
  , mIf, (~~>)
  , when, (~?>)
  , touch
  , rm
  , rmdir
  , mkdir
  , args
  , params
  , input
  , exit
  , printEnv
  , getVar
  , setVar
  , exitWith, (!?)
  , fromJust, fromMaybe, fromEither
  ) where

import Prelude hiding (when)

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.StrMap (StrMap)
import Data.Traversable (traverse)
import Data.Array (partition, zip, drop, head)
import Data.Tuple (fst, snd)

import Snail.Types as T
import Snail.Types (Snail, Script)
import Snail.OS as OS
import Snail.Path as P

import Control.Coercible (class Coercible, coerce)
import Control.Apply ((*>), (<*))
import Control.Monad.Aff (Aff, makeAff, runAff, later', attempt)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Exception (error) as Error
import Control.Monad.Error.Class (throwError)
import Control.Monad.Rec.Class (forever)

import Node.Buffer (toString)
import Node.Process (argv, exit, getEnv, setEnv, lookupEnv) as Process
import Node.ChildProcess (CHILD_PROCESS, ChildProcess, ExecResult, execFile, defaultExecOptions, spawn, defaultSpawnOptions, ignore)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readdir, appendTextFile, writeTextFile, readTextFile, unlink, stat)
import Node.FS.Aff (exists, rmdir, mkdir) as FS
import Node.FS.Stats (isFile)

-- | Runs a Snail computation
crawl :: forall e a. Snail e a -> Script e Unit
crawl = void <<< runAff (error <<< message) \ _ -> pure unit

-- | Print a string to standard output
echo :: forall e. String -> Snail e String
echo s = log s *> pure s

-- | Print a string to standard error
err :: forall e. String -> Snail e String
err s = (liftEff $ error s) *> pure s

infixl 4 bind as |>

-- | Pause the script for a given number of seconds
sleep :: forall e. Int -> Snail e Unit
sleep n = later' (n * 1000) $ pure unit

-- | Given a number of seconds n and a computation c, loop c every n seconds
loop :: forall a b e. Int -> Snail e a -> Snail e b
loop n sn = forever $ sn *> sleep n

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

-- | Given a folder, partition its contents into files and subfolders.
ls :: forall e. T.Folder -> Snail e { files :: Array String, folders :: Array String }
ls f = existsCheck f do
  let f' = T.runFolder f
  arr <- map (append f') <$> readdir f'
  stats <- traverse stat arr
  let res = partition (isFile <<< snd) $ zip arr stats
      toFilesFolders {yes, no} = {files: map fst yes, folders: map fst no}
  pure $ toFilesFolders res

-- | Append a given string to the end of a given file.
appendFile :: forall e. String -> T.File -> Snail e Unit
appendFile s f = existsCheck f $ appendTextFile UTF8 (T.runFile f) s

infixl 4 appendFile as >>

-- | Create or overwrite the given file with the given UTF8 string.
writeFile :: forall e. String -> T.File -> Snail e Unit
writeFile s f = writeTextFile UTF8 (T.runFile f) s

infixl 4 writeFile as +>

-- | Get the contents of the given file.
cat :: forall e. T.File -> Snail e String
cat f = existsCheck f $ readTextFile UTF8 (T.runFile f)

-- | Add a given string to the beginning of the given file.
prependFile :: forall e. String -> T.File -> Snail e Unit
prependFile str pth = existsCheck pth do
  content <- cat pth
  str <> "\n" <> content +> pth

-- | Run the first computation, and stop if that fails. Otherwise run the second.
andandand :: forall a b e . Snail e a -> Snail e b -> Snail e b
andandand sa sb = attempt sa >>= case _ of
  Left e -> throwError e
  _ -> sb

infixr 3 andandand as &&&

-- | Run the first computation, and stop if it succeeds.
ororor :: forall a b e. Snail e a -> Snail e b -> Snail e Unit
ororor sa sb = attempt sa >>= case _ of
  Right _ -> pure unit
  _ -> void sb

infixr 2 ororor as |||

-- | Check if a file or folder exists
exists :: forall address e. T.Address address => address -> Snail e Boolean
exists = FS.exists <<< T.getAddress

-- | Given a monadic boolean, run either the first thunked computation or the second.
mIf :: forall m a. Bind m => m Boolean -> (Unit -> m a) -> (Unit -> m a) -> m a
mIf b f g = b >>= if _
  then f unit
  else g unit

infixr 1 mIf as ~~>

-- | Run a given computation when a monadic boolean is true.
when :: forall m a. Monad m => m Boolean -> (Unit -> m a) -> m Unit
when mb ma = (mb ~~> void <<< ma) (\_ -> pure unit)

infixr 1 when as ~?>

existsCheck :: forall a e b. T.Address a => a -> Snail e b -> Snail e b
existsCheck f tr = exists f >>= if _
  then tr
  else throwError $ Error.error $ T.getAddress f <> " does not exist"

notExistsCheck :: forall a e b. T.Address a => a -> Snail e b -> Snail e b
notExistsCheck f tr = do
  e <- exists f
  if not e
     then tr
     else throwError $ Error.error $ T.getAddress f <> " already exists"

-- | Create a file with no contents.
touch :: forall e. T.File -> Snail e Unit
touch fp = not <$> exists fp ~?> \ _ -> writeFile "" fp

-- | Delete a given file.
rm :: forall e. T.File -> Snail e Unit
rm f = existsCheck f $ unlink $ T.runFile f

-- | Delete a given folder.
rmdir :: forall e. T.Folder -> Snail e Unit
rmdir f = existsCheck f $ FS.rmdir $ T.runFolder f

-- | Create a given directory.
mkdir :: forall e. T.Folder -> Snail e Unit
mkdir f = notExistsCheck f $ FS.mkdir $ T.runFolder f

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

-- | Get the current environment.
printEnv :: forall e. Snail e (StrMap String)
printEnv = liftEff Process.getEnv

-- | Set the given environment variable to the given string value.
setVar :: forall e. String -> String -> Snail e Unit
setVar k = liftEff <<< Process.setEnv k

newtype Optional a = Optional (Maybe a)

instance coercibleMaybeOptional :: Coercible (Maybe a) (Optional a) where
  coerce = Optional

instance showOptional :: Show (Optional String) where
  show (Optional (Just x)) = x
  show _ = "undefined"

-- | Get the given environment variable's value.
getVar :: forall e. String -> Snail e String
getVar s = do
  m <- liftEff $ Process.lookupEnv s
  pure $ show $ coerce m :: Optional String

-- | Exit the script with the given exit code, printing the given message to
-- | standard output if the exit code is 0, and standard error otherwise.
exitWith :: forall a e. Int -> String -> Snail e a
exitWith 0 msg = echo msg &&& exit 0
exitWith code msg = err msg &&& exit code

infix 5 exitWith as !?

-- | Extract the value of a Just, failing with a given error message otherwise.
fromJust :: forall a e. String -> Maybe a -> Snail e a
fromJust _ (Just a) = pure a
fromJust msg _ = throwError $ Error.error msg

-- | Extract the value of a Maybe with a default if Nothing is found.
fromMaybe :: forall a e. a -> Maybe a -> Snail e a
fromMaybe _ (Just a) = pure a
fromMaybe a _ = pure a

-- | Extract the Right value of an Either, otherwise turn the Left into an error.
fromEither :: forall e a b. Show a => Either a b -> Snail e b
fromEither (Left a) = throwError $ Error.error $ show a
fromEither (Right b) = pure b
