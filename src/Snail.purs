module Snail
  ( module T
  , module OS
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
import Data.Array (partition, zip, drop)
import Data.Tuple (fst, snd)

import Snail.Types as T
import Snail.Types (Snail, Script)
import Snail.OS as OS

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

crawl :: forall e a. Snail e a -> Script e Unit
crawl = void <<< runAff (error <<< message) \ _ -> pure unit

echo :: forall e. String -> Snail e String
echo s = log s *> pure s

err :: forall e. String -> Snail e String
err s = (liftEff $ error s) *> pure s

infixl 4 bind as |>

sleep :: forall e. Int -> Snail e Unit
sleep n = later' (n * 1000) $ pure unit

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

run :: forall e. String -> Array String -> Snail e String
run cmd as = do
  res <- runFileImp cmd as
  liftEff $ toString UTF8 res.stdout

command :: forall e. String -> Array String -> Snail e Unit
command cmd = void <<< run cmd

foreign import unref :: forall e. ChildProcess -> Script e Unit

fork :: forall e. String -> Array String -> Snail e Unit
fork cmd as = liftEff <<< unref
          <=< liftEff $ spawn cmd as (defaultSpawnOptions {stdio = ignore, detached = true})

ls :: forall e. T.Folder -> Snail e { files :: Array String, folders :: Array String }
ls f = existsCheck f do
  let f' = T.runFolder f
  arr <- map (append f') <$> readdir f'
  stats <- traverse stat arr
  let res = partition (isFile <<< snd) $ zip arr stats
      toFilesFolders {yes, no} = {files: map fst yes, folders: map fst no}
  pure $ toFilesFolders res

appendFile :: forall e. String -> T.File -> Snail e Unit
appendFile s f = existsCheck f $ appendTextFile UTF8 (T.runFile f) s

infixl 4 appendFile as >>

writeFile :: forall e. String -> T.File -> Snail e Unit
writeFile s f = existsCheck f $ writeTextFile UTF8 (T.runFile f) s

infixl 4 writeFile as +>

cat :: forall e. T.File -> Snail e String
cat f = existsCheck f $ readTextFile UTF8 (T.runFile f)

prependFile :: forall e. String -> T.File -> Snail e Unit
prependFile str pth = cat pth |> \c -> writeFile (str <> "\n" <> c) pth

andandand :: forall a b e . Snail e a -> Snail e b -> Snail e b
andandand sa sb = attempt sa >>= case _ of
  Left e -> throwError e
  _ -> sb

infixr 3 andandand as &&&

ororor :: forall a b e. Snail e a -> Snail e b -> Snail e Unit
ororor sa sb = do
  a <- attempt sa
  case a of
       Right _ -> pure unit
       _ -> sb *> pure unit

infixr 2 ororor as |||

exists :: forall address e. T.Address address => address -> Snail e Boolean
exists = FS.exists <<< T.getAddress

mIf :: forall m a. Bind m => m Boolean -> (Unit -> m a) -> (Unit -> m a) -> m a
mIf b f g = do
  b' <- b
  if b'
     then f unit
     else g unit

infixr 1 mIf as ~~>

when :: forall m a. Monad m => m Boolean -> m a -> m Unit
when mb ma = (mb ~~> \_ -> void ma) (\_ -> pure unit)

infixr 0 when as ~?>

existsCheck :: forall a e b. T.Address a => a -> Snail e b -> Snail e b
existsCheck f tr = do
  e <- exists f
  if e
     then tr
     else throwError $ Error.error $ T.getAddress f <> " does not exist"

notExistsCheck :: forall a e b. T.Address a => a -> Snail e b -> Snail e b
notExistsCheck f tr = do
  e <- exists f
  if not e
     then tr
     else throwError $ Error.error $ T.getAddress f <> " already exists"

touch :: forall e. T.File -> Snail e Unit
touch fp = not <$> exists fp ~?> writeFile "" fp

rm :: forall e. T.File -> Snail e Unit
rm f = existsCheck f $ unlink $ T.runFile f

rmdir :: forall e. T.Folder -> Snail e Unit
rmdir f = existsCheck f $ FS.rmdir $ T.runFolder f

mkdir :: forall e. T.Folder -> Snail e Unit
mkdir f = notExistsCheck f $ FS.mkdir $ T.runFolder f

args :: forall e. Snail e (Array String)
args = liftEff Process.argv

params :: forall e. Snail e (Array String)
params = drop 2 <$> args

exit :: forall a e. Int -> Snail e a
exit = liftEff <<< Process.exit

printEnv :: forall e. Snail e (StrMap String)
printEnv = liftEff Process.getEnv

setVar :: forall e. String -> String -> Snail e Unit
setVar k = liftEff <<< Process.setEnv k

newtype Optional a = Optional (Maybe a)

instance coercibleMaybeOptional :: Coercible (Maybe a) (Optional a) where
  coerce = Optional

instance showOptional :: Show (Optional String) where
  show (Optional (Just x)) = x
  show _ = "undefined"

getVar :: forall e. String -> Snail e String
getVar s = do
  m <- liftEff $ Process.lookupEnv s
  pure $ show $ coerce m :: Optional String

exitWith :: forall a e. Int -> String -> Snail e a
exitWith 0 msg = echo msg &&& exit 0
exitWith code msg = err msg &&& exit code

infix 0 exitWith as !?

fromJust :: forall a e. String -> Maybe a -> Snail e a
fromJust _ (Just a) = pure a
fromJust msg _ = throwError $ Error.error msg

fromMaybe :: forall a e. a -> Maybe a -> Snail e a
fromMaybe _ (Just a) = pure a
fromMaybe a _ = pure a

fromEither :: forall e a b. Show a => Either a b -> Snail e b
fromEither (Left a) = throwError $ Error.error $ show a
fromEither (Right b) = pure b
